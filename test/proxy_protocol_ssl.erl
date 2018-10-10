-module(proxy_protocol_ssl).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

start_link(Ref, _Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
	{ok, Pid}.

init(Ref, Transport, _Opts = []) ->
	{ok, Socket} = ranch:handshake(Ref),
	{ok, ProxyInfo} = Transport:recv_proxy_header(Socket, 1000),
	Pid = ct_helper:get_remote_pid_tcp(Socket),
	Pid ! {?MODULE, ProxyInfo},
	Opts = ct_helper:get_certs_from_ets(),
	{ok, SslSocket} = ranch_ssl:handshake(Socket, Opts, 1000),
	loop(SslSocket, ranch_ssl).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			_ = Transport:send(Socket, Data),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.
