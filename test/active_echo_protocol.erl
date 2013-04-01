-module(active_echo_protocol).
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
	{OK, Closed, Error} = Transport:messages(),
	Transport:setopts(Socket, [{active, once}]),
	receive
		{OK, Socket, Data} ->
			Transport:send(Socket, Data),
			loop(Socket, Transport);
		{Closed, Socket} ->
			ok;
		{Error, Socket, _} ->
			ok = Transport:close(Socket)
	end.
