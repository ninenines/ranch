-module(remove_conn_and_wait_protocol).

-export([start_link/4]).
-export([init/2]).

start_link(ListenerPid, _, _, [{remove, MaybeRemove}]) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, MaybeRemove]),
	{ok, Pid}.

init(ListenerPid, MaybeRemove) ->
	ranch:accept_ack(ListenerPid),
	case MaybeRemove of
		true ->
			ranch_listener:remove_connection(ListenerPid);
		false ->
			ok
	end,
	receive after 2500 -> ok end.
