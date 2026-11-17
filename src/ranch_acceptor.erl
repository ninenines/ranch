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

-record(state, {ref :: ranch:ref(),
		id :: pos_integer(),
		listen_socket :: inet:socket(),
		transport :: module(),
		conns_sup :: {pid(), reference()} | undefined,
		logger :: module()}).

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
	State0 = #state{ref = Ref,
			id = AcceptorId,
			listen_socket = LSocket,
			transport = Transport,
			logger = Logger},
	State1 = update_conns_sup_or_fail(State0, no_conns_sup),
	loop(State1).

-spec loop(#state{}) -> no_return().
loop(State0 = #state{transport = Transport, listen_socket = LSocket, logger = Logger}) ->
	State1 = case Transport:accept(LSocket, infinity) of
			 {ok, CSocket} ->
				 socket_accepted(State0, CSocket);
			 {error, Reason} ->
				 handle_accept_error(Logger, Reason),
				 State0
		 end,
	State2 = flush(State1),
	?MODULE:loop(State2).

socket_accepted(State = #state{transport = Transport, conns_sup = {ConnsSupPid, _}}, CSocket) ->
	case Transport:controlling_process(CSocket, ConnsSupPid) of
		ok ->
			start_protocol(State, CSocket);
		{error, _} ->
			Transport:close(CSocket),
			State
	end.

handle_accept_error(Logger, Reason) when Reason =:= emfile; Reason =:= enfile ->
	ranch:log(warning,
		  "Ranch acceptor reducing accept rate: out of file descriptors~n",
		  [],
		  Logger),
	receive after 100 -> ok end;
handle_accept_error(_, closed) ->
	exit(closed);
handle_accept_error(_, _) ->
	ok.

start_protocol(State = #state{conns_sup = {ConnsSupPid, ConnsSupMonitor}}, CSocket) ->
	case ranch_conns_sup:start_protocol(ConnsSupPid, ConnsSupMonitor, CSocket) of
		ok ->
			State;
		{error, Reason} ->
			update_conns_sup_or_fail(State, Reason)
	end.

flush(State) ->
	case do_flush(State, ok) of
		ok ->
			State;
		{error, Reason} ->
			update_conns_sup_or_fail(State, Reason)
	end.

do_flush(State = #state{logger = Logger, conns_sup = {ConnsSupPid, ConnsSupMonitor}}, Res) ->
	receive
		{'DOWN', ConnsSupMonitor, process, ConnsSupPid, Reason} ->
			do_flush(State, {error, Reason});
		Msg ->
			ranch:log(warning,
				"Ranch acceptor received unexpected message: ~p~n",
				[Msg], Logger),
			do_flush(State, Res)
	after 0 ->
		Res
	end.

update_conns_sup_or_fail(State = #state{ref = Ref, id = Id}, Reason) ->
	case get_conns_sup(Ref, Id) of
		undefined ->
			error(Reason);
		ConnsSup ->
			State#state{conns_sup = ConnsSup}
	end.

get_conns_sup(Ref, AcceptorId) ->
	get_conns_sup(Ref, AcceptorId, 10).

get_conns_sup(Ref, AcceptorId, N) ->
	case ranch_server:get_connections_sup(Ref, AcceptorId) of
		undefined when N =< 1 ->
			undefined;
		undefined ->
			receive after 100 -> ok end,
			get_conns_sup(Ref, AcceptorId, N - 1);
		ConnsSup when is_pid(ConnsSup) ->
			MonitorRef = monitor(process, ConnsSup),
			{ConnsSup, MonitorRef}
	end.
