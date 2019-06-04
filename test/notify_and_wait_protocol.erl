-module(notify_and_wait_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/4]).

start_link(_, _, Opts = #{pid := TestPid}) ->
	Msg = maps:get(msg, Opts, connected),
	TerminateMsg = maps:get(terminate_msg, Opts, stop),
	Timeout = maps:get(timeout, Opts, infinity),
	Pid = spawn_link(?MODULE, init, [Msg, TestPid, TerminateMsg, Timeout]),
	{ok, Pid}.

init(Msg, Pid, TerminateMsg, Timeout) ->
	Pid ! {self(), Msg},
	receive TerminateMsg -> ok after Timeout -> ok end.
