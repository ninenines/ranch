-module(ssl_upgrade_protocol).
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
		{ok, <<"UPGRADE">>} when Transport=:=ranch_tcp ->
			ok = Transport:send(Socket, <<"READY">>),
			Opts=ct_helper:get_certs_from_ets(),
			case ranch_ssl:upgrade(Socket, Transport, [{verify, verify_none}|Opts], 1000) of
				{ok, {NewSocket, NewTransport}} ->
					loop(NewSocket, NewTransport);

				_ ->
					ok = Transport:close(Socket)
			end;

		{ok, <<"ECHO ", More/binary>>} ->
			ok = Transport:send(Socket, More),
			loop(Socket, Transport);

		_ ->
			ok = Transport:close(Socket)
	end.
