%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(acceptor_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% ssl.
-export([ssl_accept_error/1]).
-export([ssl_active_echo/1]).
-export([ssl_echo/1]).

%% tcp.
-export([tcp_active_echo/1]).
-export([tcp_echo/1]).
-export([tcp_max_connections/1]).
-export([tcp_max_connections_and_beyond/1]).
-export([tcp_upgrade/1]).
-export([tcp_sync_accept/1]).
-export([tcp_sync_accept_handle/1]).

%% ct.

all() ->
	[{group, tcp}, {group, ssl}].

groups() ->
	[{tcp, [
		tcp_active_echo,
		tcp_echo,
		tcp_max_connections,
		tcp_max_connections_and_beyond,
		tcp_upgrade,
		tcp_sync_accept,
		tcp_sync_accept_handle
	]}, {ssl, [
		ssl_accept_error,
		ssl_active_echo,
		ssl_echo
	]}].

init_per_suite(Config) ->
	application:start(ranch),
	Config.

end_per_suite(_) ->
	application:stop(ranch),
	ok.

init_per_group(ssl, Config) ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	Config;
init_per_group(_, Config) ->
	Config.



end_per_group(ssl, _) ->
	application:stop(ssl),
	application:stop(public_key),
	application:stop(crypto),
	ok;
end_per_group(_, _) ->
	ok.

%% ssl.

ssl_accept_error(Config) ->
	{ok, _} = ranch:start_listener(ssl_accept_error, 1,
		ranch_ssl, [{port, 0},
			{certfile, ?config(data_dir, Config) ++ "cert.pem"}],
		echo_protocol, []),
	Port = ranch:get_port(ssl_accept_error),
	[AcceptorPid] = ets:lookup_element(ranch_server,
		{acceptors, ssl_accept_error}, 2),
	true = is_process_alive(AcceptorPid),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:close(Socket),
	receive after 500 -> ok end,
	true = is_process_alive(AcceptorPid).

ssl_active_echo(Config) ->
	{ok, _} = ranch:start_listener(ssl_active_echo, 1,
		ranch_ssl, [{port, 0},
			{certfile, ?config(data_dir, Config) ++ "cert.pem"}],
		active_echo_protocol, []),
	Port = ranch:get_port(ssl_active_echo),
	{ok, Socket} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw},
		{certfile, ?config(data_dir, Config) ++ "cert.pem"}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(ssl_active_echo),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(ssl_active_echo) end,
	ok.

ssl_echo(Config) ->
	{ok, _} = ranch:start_listener(ssl_echo, 1,
		ranch_ssl, [{port, 0},
			{certfile, ?config(data_dir, Config) ++ "cert.pem"}],
		echo_protocol, []),
	Port = ranch:get_port(ssl_echo),
	{ok, Socket} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw},
		{certfile, ?config(data_dir, Config) ++ "cert.pem"}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(ssl_echo),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(ssl_echo) end,
	ok.

%% tcp.

tcp_active_echo(_) ->
	{ok, _} = ranch:start_listener(tcp_active_echo, 1,
		ranch_tcp, [{port, 0}], active_echo_protocol, []),
	Port = ranch:get_port(tcp_active_echo),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(tcp_active_echo),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(tcp_active_echo) end,
	ok.

tcp_echo(_) ->
	{ok, _} = ranch:start_listener(tcp_echo, 1,
		ranch_tcp, [{port, 0}], echo_protocol, []),
	Port = ranch:get_port(tcp_echo),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(tcp_echo),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(tcp_echo) end,
	ok.

tcp_sync_accept(_) ->
	{ok, _} = ranch:start_listener(tcp_sync_accept, 1,
		ranch_tcp, [{port, 0}], echo_protocol, [], [{sync_accept, true}]),
	Port = ranch:get_port(tcp_sync_accept),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch with sync accept is working!">>),
	{ok, <<"TCP Ranch with sync accept is working!">>} = gen_tcp:recv(Socket, 21 + 17, 1000),
	ok = ranch:stop_listener(tcp_sync_accept),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(tcp_sync_accept) end,
	ok.

tcp_sync_accept_handle(_) ->
	{ok, _} = ranch:start_listener(tcp_sync_accept_handle, 1,
		ranch_tcp, [{port, 0}], echo_protocol, [], [{sync_accept, true}, {sync_handle, true}]),
	Port = ranch:get_port(tcp_sync_accept_handle),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch with sync accept and handle is working!">>),
	{ok, <<"TCP Ranch with sync accept and handle is working!">>} = gen_tcp:recv(Socket, 21 + 17 + 11, 1000),
	ok = ranch:stop_listener(tcp_sync_accept_handle),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(tcp_sync_accept_handle) end,
	ok.

tcp_max_connections(_) ->
	{ok, _} = ranch:start_listener(tcp_max_connections, 1,
		ranch_tcp, [{port, 0}, {max_connections, 10}],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(tcp_max_connections),
	%% @todo We'll probably want a more direct interface to count_connections.
	ListenerPid = ranch_server:lookup_listener(tcp_max_connections),
	ok = connect_loop(Port, 11, 150),
	10 = ranch_server:count_connections(ListenerPid),
	10 = receive_loop(connected, 400),
	1 = receive_loop(connected, 1000).

tcp_max_connections_and_beyond(_) ->
	{ok, _} = ranch:start_listener(tcp_max_connections_and_beyond, 1,
		ranch_tcp, [{port, 0}, {max_connections, 10}],
		remove_conn_and_wait_protocol, [{remove, true}]),
	Port = ranch:get_port(tcp_max_connections_and_beyond),
	%% @todo We'll probably want a more direct interface to count_connections.
	ListenerPid = ranch_server:lookup_listener(tcp_max_connections_and_beyond),
	ok = connect_loop(Port, 10, 0),
	0 = ranch_server:count_connections(ListenerPid),
	ranch:set_protocol_options(tcp_max_connections_and_beyond,
		[{remove, false}]),
	receive after 500 -> ok end,
	ok = connect_loop(Port, 10, 0),
	receive after 500 -> ok end,
	10 = ranch_server:count_connections(ListenerPid).

tcp_upgrade(_) ->
	receive after 20000 -> ok end,
	{ok, _} = ranch:start_listener(tcp_upgrade, 1,
		ranch_tcp, [{port, 0}],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(tcp_upgrade),
	ok = connect_loop(Port, 1, 0),
	receive connected -> ok after 1000 -> error(timeout) end,
	ranch:set_protocol_options(tcp_upgrade, [{msg, upgraded}, {pid, self()}]),
	ok = connect_loop(Port, 1, 0),
	receive upgraded -> ok after 1000 -> error(timeout) end.

%% Utility functions.

connect_loop(_, 0, _) ->
	ok;
connect_loop(Port, N, Sleep) ->
	{ok, _} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	receive after Sleep -> ok end,
	connect_loop(Port, N - 1, Sleep).

receive_loop(Message, Timeout) ->
	receive_loop(Message, Timeout, 0).
receive_loop(Message, Timeout, N) ->
	receive Message ->
		receive_loop(Message, Timeout, N + 1)
	after Timeout ->
		N
	end.
