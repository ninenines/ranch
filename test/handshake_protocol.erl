-module(handshake_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, Opts) ->
	SniHost = case ranch:handshake(Ref) of
		%% Due to a bug in ssl (https://bugs.erlang.org/browse/ERL-951,
		%% fixed in OTP 22.0.3) the value for sni may be {sni, Hostname}
		%% instead of Hostname.
		{continue, #{sni := {sni, Hostname}}} ->
			Hostname;
		{continue, #{sni := Hostname}} ->
			Hostname
	end,
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
