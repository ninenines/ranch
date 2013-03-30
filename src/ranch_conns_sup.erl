%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @private
-module(ranch_conns_sup).

%% API.
-export([start_link/3]).
-export([start_protocol/2]).
-export([active_connections/1]).

%% Supervisor internals.
-export([init/4]).
-export([system_continue/3]).
-export([system_terminate/4]).
-export([system_code_change/4]).

-record(state, {
	parent = undefined :: pid(),
	listener_pid = undefined :: pid(),
	transport = undefined :: module(),
	protocol = undefined :: module(),
	opts :: any(),
	max_conns = undefined :: non_neg_integer() | infinity
}).

%% API.

-spec start_link(any(), module(), module()) -> {ok, pid()}.
start_link(Ref, Transport, Protocol) ->
	proc_lib:start_link(?MODULE, init, [self(), Ref, Transport, Protocol]).

%% We can safely assume we are on the same node as the supervisor.
%%
%% We can also safely avoid having a monitor and a timeout here
%% because only three things can happen:
%%  *  The supervisor died; rest_for_one strategy killed all acceptors
%%     so this very calling process is going to di--
%%  *  There's too many connections, the supervisor will resume the
%%     acceptor only when we get below the limit again.
%%  *  The supervisor is overloaded, there's either too many acceptors
%%     or the max_connections limit is too large. It's better if we
%%     don't timeout and keep accepting connections because this leaves
%%     more room for the situation to be resolved.
-spec start_protocol(pid(), inet:socket()) -> {ok, pid()}.
start_protocol(SupPid, Socket) ->
	SupPid ! {?MODULE, start_protocol, self(), Socket},
	receive
		{?MODULE, Ret} ->
			Ret
	end.

%% We can't make the above assumptions here. This function might be
%% called from anywhere.
-spec active_connections(pid()) -> non_neg_integer().
active_connections(SupPid) ->
	Tag = erlang:monitor(process, SupPid),
	erlang:send(SupPid, {?MODULE, active_connections, self(), Tag},
		[noconnect]),
	receive
		{Tag, Ret} ->
			erlang:demonitor(Tag, [flush]),
			Ret;
		{'DOWN', Tag, _, _, noconnection} ->
			exit({nodedown, node(SupPid)});
		{'DOWN', Tag, _, _, Reason} ->
			exit(Reason)
	after 5000 ->
		erlang:demonitor(Tag, [flush]),
		exit(timeout)
	end.

%% Supervisor internals.

-spec init(pid(), any(), module(), module()) -> no_return().
init(Parent, Ref, Transport, Protocol) ->
	process_flag(trap_exit, true),
	ok = ranch_server:set_connections_sup(Ref, self()),
	ListenerPid = ranch_server:lookup_listener(Ref),
	{ok, MaxConns} = ranch_listener:get_max_connections(ListenerPid),
	{ok, Opts} = ranch_listener:get_protocol_options(ListenerPid),
	ok = proc_lib:init_ack(Parent, {ok, self()}),
	loop(#state{parent=Parent, listener_pid=ListenerPid, transport=Transport,
		protocol=Protocol, opts=Opts, max_conns=MaxConns}, 0, 0, []).

loop(State=#state{parent=Parent, listener_pid=ListenerPid,
		transport=Transport, protocol=Protocol, opts=Opts,
		max_conns=MaxConns}, CurConns, NbChildren, Sleepers) ->
	receive
		{?MODULE, start_protocol, To, Socket} ->
			case Protocol:start_link(ListenerPid, Socket, Transport, Opts) of
				Ret = {ok, Pid} ->
					Transport:controlling_process(Socket, Pid),
					Pid ! {shoot, ListenerPid},
					put(Pid, true),
					CurConns2 = CurConns + 1,
					if CurConns2 < MaxConns ->
							To ! {?MODULE, Ret},
							loop(State, CurConns2, NbChildren + 1,
								Sleepers);
						true ->
							loop(State, CurConns2, NbChildren + 1,
								[{To, Ret}|Sleepers])
					end;
				Ret ->
					To ! {?MODULE, Ret},
					loop(State, CurConns, NbChildren, Sleepers)
			end;
		{?MODULE, active_connections, To, Tag} ->
			To ! {Tag, CurConns},
			loop(State, CurConns, NbChildren, Sleepers);
		%% Remove a connection from the count of connections.
		remove_connection ->
			loop(State, CurConns - 1, NbChildren, Sleepers);
		%% Upgrade the max number of connections allowed concurrently.
		%% We resume all sleeping acceptors if this number increases.
		{set_max_conns, MaxConns2} when MaxConns2 > MaxConns ->
			_ = [To ! {?MODULE, Ret} || {To, Ret} <- Sleepers],
			loop(State#state{max_conns=MaxConns2},
				CurConns, NbChildren, []);
		{set_max_conns, MaxConns2} ->
			loop(State#state{max_conns=MaxConns2},
				CurConns, NbChildren, Sleepers);
		%% Upgrade the protocol options.
		{set_opts, Opts2} ->
			loop(State#state{opts=Opts2},
				CurConns, NbChildren, Sleepers);
		{'EXIT', Parent, Reason} ->
			exit(Reason);
		{'EXIT', Pid, _} when Sleepers =:= [] ->
			erase(Pid),
			loop(State, CurConns - 1, NbChildren - 1, Sleepers);
		%% Resume a sleeping acceptor if needed.
		{'EXIT', Pid, _} ->
			erase(Pid),
			[{To, Ret}|Sleepers2] = Sleepers,
			To ! {?MODULE, Ret},
			loop(State, CurConns - 1, NbChildren - 1, Sleepers2);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, [],
				{State, CurConns, NbChildren, Sleepers});
		%% Calls from the supervisor module.
		{'$gen_call', {To, Tag}, which_children} ->
			Pids = get_keys(true),
			Children = [{Protocol, Pid, worker, [Protocol]}
				|| Pid <- Pids],
			To ! {Tag, Children},
			loop(State, CurConns, NbChildren, Sleepers);
		{'$gen_call', {To, Tag}, count_children} ->
			Counts = [{specs, 1}, {active, NbChildren},
				{supervisors, 0}, {workers, NbChildren}],
			To ! {Tag, Counts},
			loop(State, CurConns, NbChildren, Sleepers);
		{'$gen_call', {To, Tag}, _} ->
			To ! {Tag, {error, ?MODULE}},
			loop(State, CurConns, NbChildren, Sleepers)
	end.

system_continue(_, _, {State, CurConns, NbChildren, Sleepers}) ->
	loop(State, CurConns, NbChildren, Sleepers).

-spec system_terminate(any(), _, _, _) -> no_return().
system_terminate(Reason, _, _, _) ->
	exit(Reason).

system_code_change(Misc, _, _, _) ->
	{ok, Misc}.
