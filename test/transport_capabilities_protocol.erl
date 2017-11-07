-module(transport_capabilities_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
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
	case catch Transport:getopts(Socket, []) of
		{ok, _} ->
			ok;
		_ ->
			error
	end;

check(Socket, Transport, <<"getstat/1">>) ->
	case catch Transport:getstat(Socket) of
		{ok, _} ->
			ok;
		_ ->
			error
	end;

check(Socket, Transport, <<"getstat/2">>) ->
	case catch Transport:getstat(Socket, []) of
		{ok, _} ->
			ok;
		_ ->
			error
	end.
