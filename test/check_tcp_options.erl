-module(check_tcp_options).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, _, [{pid, TestPid}|TcpOptions]) ->
	Pid = spawn_link(?MODULE, init, [Ref, TestPid, TcpOptions]),
	{ok, Pid}.

init(Ref, Pid, TcpOptions) ->
	{ok, Socket} = ranch:handshake(Ref),
	{ok, RealTcpOptions} = inet:getopts(Socket, [Key || {Key, _} <- TcpOptions]),
	true = TcpOptions =:= RealTcpOptions,
	Pid ! checked,
	receive after 2500 -> ok end.
