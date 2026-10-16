-module(ssl_info_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Pid) ->
	{ok, spawn_link(?MODULE, init, [Ref, Transport, Pid])}.

init(Ref, Transport, Pid) ->
	{ok, Socket} = ranch:handshake(Ref),
	Pid ! {self(), Transport:peercert(Socket),
		Transport:negotiated_protocol(Socket)},
	ok = Transport:close(Socket).
