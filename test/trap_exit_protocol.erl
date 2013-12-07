-module(trap_exit_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	process_flag(trap_exit, true),
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, infinity) of
		{ok, Data} ->
			Transport:send(Socket, Data),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.
