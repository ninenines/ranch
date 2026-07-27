-module(transport_capabilities_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, _Opts = []) ->
	{ok, Socket} = ranch:handshake(Ref),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			Reply =
			case check(Socket, Transport, Data) of
				ok ->
					<<"OK">>;
				error ->
					<<"ERROR">>
			end,
			Transport:send(Socket, Reply),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.

check(Socket, Transport, <<"getopts/2">>) ->
	try Transport:getopts(Socket, []) of
		{ok, _} ->
			ok;
		_ ->
			error
	catch _:_ ->
		error
	end;

check(Socket, Transport, <<"getstat/1">>) ->
	try Transport:getstat(Socket) of
		{ok, _} ->
			ok;
		_ ->
			error
	catch _:_ ->
		error
	end;

check(Socket, Transport, <<"getstat/2">>) ->
	try Transport:getstat(Socket, []) of
		{ok, _} ->
			ok;
		_ ->
			error
	catch _:_ ->
		error
	end.
