-module(handshake_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, Opts) ->
	{continue, #{sni := SniHost}} = ranch:handshake(Ref),
	SniHostOpts = maps:get(SniHost, Opts),
	{ok, Socket} = ranch:handshake_continue(Ref, SniHostOpts),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			Transport:send(Socket, Data),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.
