%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_acceptor).

-export([start_link/5]).
-export([init/5]).
-export([loop/1]).

-record(state, {
	ref :: ranch:ref(),
	%% Id of the connection supervisor this acceptor uses.
	%% It is fixed at start from num_conns_sups. The count
	%% in ranch_server shrinks while one supervisor is down.
	slot :: pos_integer(),
	lsocket :: inet:socket(),
	transport :: module(),
	logger :: module(),
	conns_sup :: pid(),
	%% Monitor of conns_sup. A DOWN for this reference
	%% means that supervisor died.
	monitor_ref :: reference()
}).

-spec start_link(ranch:ref(), pos_integer(), pos_integer(), module(), module())
	-> {ok, pid()}.
start_link(Ref, AcceptorId, LSocketId, Transport, Logger) ->
	LSocket = ranch_acceptors_sup:get_listen_socket(LSocketId),
	Pid = spawn_link(?MODULE, init, [Ref, AcceptorId, LSocket, Transport, Logger]),
	{ok, Pid}.

-spec init(ranch:ref(), pos_integer(), inet:socket(), module(), module())
	-> no_return().
init(Ref, AcceptorId, LSocket, Transport, Logger) ->
	proc_lib:set_label({ranch_acceptor, Ref, AcceptorId}),
	%% The id is fixed here. ranch_server drops the row while
	%% a supervisor is down, so length/1 would pick another id.
	TransOpts = ranch_server:get_transport_options(Ref),
	NumAcceptors = maps:get(num_acceptors, TransOpts, 10),
	NumConnsSups = maps:get(num_conns_sups, TransOpts, NumAcceptors),
	Slot = (AcceptorId rem NumConnsSups) + 1,
	{Slot, ConnsSup} = lists:keyfind(Slot, 1,
		ranch_server:get_connections_sups(Ref)),
	loop(#state{ref=Ref, slot=Slot, lsocket=LSocket, transport=Transport,
		logger=Logger, conns_sup=ConnsSup,
		monitor_ref=monitor(process, ConnsSup)}).

-spec loop(#state{}) -> no_return().
loop(State0=#state{lsocket=LSocket, transport=Transport, logger=Logger,
		conns_sup=ConnsSup, monitor_ref=MonitorRef}) ->
	State1 = case Transport:accept(LSocket, infinity) of
		{ok, CSocket} ->
			case Transport:controlling_process(CSocket, ConnsSup) of
				ok ->
					%% This call will not return until process has been started
					%% AND we are below the maximum number of connections.
					case ranch_conns_sup:start_protocol(ConnsSup, MonitorRef,
						CSocket) of
						ok ->
							State0;
						{error, Reason} ->
							maybe_update_conns_sup(State0, Reason)
					end;
				{error, _} ->
					Transport:close(CSocket),
					State0
			end;
		%% Reduce the accept rate if we run out of file descriptors.
		%% We can't accept anymore anyway, so we might as well wait
		%% a little for the situation to resolve itself.
		{error, Reason} when Reason =:= emfile; Reason =:= enfile;
			Reason =:= system_limit ->
			ranch:log(warning,
				"Ranch acceptor reducing accept rate: out of file descriptors~n",
				[], Logger),
			receive after 100 -> ok end,
			State0;
		%% Exit if the listening socket got closed.
		{error, closed} ->
			exit(closed);
		%% Continue otherwise.
		{error, _} ->
			State0
	end,
	State = flush(State1),
	?MODULE:loop(State).

flush(State) ->
	flush(State, ok).

%% A DOWN is remembered while the rest of the mailbox is logged.
%% The replacement is resolved only after that.
flush(State=#state{logger=Logger, conns_sup=ConnsSup, monitor_ref=MonitorRef},
		Res) ->
	receive
		{'DOWN', MonitorRef, process, ConnsSup, Reason} ->
			flush(State, {error, Reason});
		Msg ->
			ranch:log(warning,
				"Ranch acceptor received unexpected message: ~p~n",
				[Msg], Logger),
			flush(State, Res)
	after 0 ->
		case Res of
			ok ->
				State;
			{error, Reason} ->
				maybe_update_conns_sup(State, Reason)
		end
	end.

%% The new supervisor registers after ranch_server drops the old pid.
maybe_update_conns_sup(State, Reason) ->
	maybe_update_conns_sup(State, Reason, 10).

maybe_update_conns_sup(State=#state{ref=Ref, slot=Slot,
		monitor_ref=MonitorRef}, Reason, N) ->
	ConnsSup = case lists:keyfind(Slot, 1,
			ranch_server:get_connections_sups(Ref)) of
		{_, Pid} ->
			Pid;
		false ->
			undefined
	end,
	case is_pid(ConnsSup) andalso erlang:is_process_alive(ConnsSup) of
		true ->
			%% The old DOWN may still be in the mailbox. Drop it
			%% before the state changes, or flush logs it.
			_ = demonitor(MonitorRef, [flush]),
			State#state{conns_sup=ConnsSup,
				monitor_ref=monitor(process, ConnsSup)};
		false when N =< 1 ->
			error(Reason);
		false ->
			receive after 100 -> ok end,
			maybe_update_conns_sup(State, Reason, N - 1)
	end.
