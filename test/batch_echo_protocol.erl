-module(batch_echo_protocol).
-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, [{batch_size, N}]) ->
	Pid = spawn_link(?MODULE, init, [Ref, Transport, N]),
	{ok, Pid}.

init(Ref, Transport, N) ->
	{ok, Socket} = ranch:handshake(Ref),
	Transport:setopts(Socket, [{active, N}]),
	loop(Socket, Transport, N, <<>>).

loop(Socket, Transport, N, Acc) ->
	{OK, Closed, Error, Passive} = Transport:messages(),
	receive
		{OK, Socket, Data} ->
			Transport:send(Socket, <<"OK">>),
			loop(Socket, Transport, N, <<Acc/binary, Data/binary>>);
		{Passive, Socket} ->
			Transport:send(Socket, Acc),
			Transport:setopts(Socket, [{active, N}]),
			loop(Socket, Transport, N, <<>>);
		{Closed, Socket} ->
			ok;
		{Error, Socket, _} ->
			ok = Transport:close(Socket)
	end.
