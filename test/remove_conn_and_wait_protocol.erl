-module(remove_conn_and_wait_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/4]).

start_link(Ref, _, [{remove, MaybeRemove, Timeout}]) ->
	Pid = spawn_link(?MODULE, init, [Ref, MaybeRemove, Timeout, self]),
	{ok, Pid};
start_link(Ref, _, [{remove, MaybeRemove, Timeout, To}]) ->
	Pid = spawn_link(?MODULE, init, [Ref, MaybeRemove, Timeout, To]),
	{ok, Pid}.

init(Ref, MaybeRemove, Timeout, To) ->
	{ok, _} = ranch:handshake(Ref),
	_ = case MaybeRemove of
		true ->
			remove_connection(Ref, To);
		false ->
			ok;
		N ->
			[remove_connection(Ref, To) || _ <- lists:seq(1, N)]
	end,
	receive after Timeout -> ok end.

%% `self` keeps the worker tests on remove_connection/1.
%% A pid is the supervised parent and uses remove_connection/2.
remove_connection(Ref, self) ->
	ranch:remove_connection(Ref);
remove_connection(Ref, Pid) ->
	ranch:remove_connection(Ref, Pid).
