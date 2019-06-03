-module(notify_and_wait_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(_, _, Opts = #{msg := Msg, pid := TestPid}) ->
	Timeout = maps:get(timeout, Opts, 2500),
	Pid = spawn_link(?MODULE, init, [Msg, TestPid, Timeout]),
	{ok, Pid}.

init(Msg, Pid, Timeout) ->
	Pid ! Msg,
	receive after Timeout -> ok end.
