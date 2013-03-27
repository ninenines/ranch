-module(remove_conn_and_wait_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(ListenerPid, Socket, Transport, [{remove, MaybeRemove}]) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, MaybeRemove]),
	{ok, Pid}.

init(ListenerPid, Socket, Transport, MaybeRemove) ->
	_ = ranch:accept_ack(ListenerPid, Socket, Transport, infinity),
	case MaybeRemove of
		true ->
			ranch_listener:remove_connection(ListenerPid);
		false ->
			ok
	end,
	receive after 2500 -> ok end.
