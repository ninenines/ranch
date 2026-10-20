-module(proxy_header_result).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, #{pid := TestPid}) ->
	Peer = recv_peer(Ref),
	Result = ranch:recv_proxy_header(Ref, 1000),
	TestPid ! {result, Peer, Result},
	case Result of
		{ok, _} ->
			{ok, Socket} = ranch:handshake(Ref),
			ok = Transport:close(Socket);
		_ ->
			ok
	end.

recv_peer(Ref) ->
	receive
		{handshake, Ref, Transport, Socket, _} = Msg ->
			self() ! Msg,
			case Transport:peername(Socket) of
				{ok, Peer} ->
					Peer;
				{error, _} ->
					undefined
			end
	after 2000 ->
		undefined
	end.
