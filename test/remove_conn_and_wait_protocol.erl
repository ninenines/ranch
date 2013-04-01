-module(remove_conn_and_wait_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/2]).

start_link(Ref, _, _, [{remove, MaybeRemove}]) ->
	Pid = spawn_link(?MODULE, init, [Ref, MaybeRemove]),
	{ok, Pid}.

init(Ref, MaybeRemove) ->
	ranch:accept_ack(Ref),
	case MaybeRemove of
		true ->
			ranch:remove_connection(Ref);
		false ->
			ok
	end,
	receive after 2500 -> ok end.
