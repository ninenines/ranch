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

%% misc.
-export([misc_bad_transport/1]).

%% ssl.
-export([ssl_accept_error/1]).
-export([ssl_accept_socket/1]).
-export([ssl_active_echo/1]).
-export([ssl_echo/1]).

%% tcp.
-export([tcp_accept_socket/1]).
-export([tcp_active_echo/1]).
-export([tcp_echo/1]).
-export([tcp_max_connections/1]).
-export([tcp_max_connections_and_beyond/1]).
-export([tcp_upgrade/1]).

%% supervisor.
-export([supervisor_clean_restart/1]).
-export([supervisor_clean_child_restart/1]).
-export([supervisor_conns_alive/1]).

%% ct.

all() ->
	[{group, tcp}, {group, ssl}, {group, misc}, {group, supervisor}].

groups() ->
	[{tcp, [
		tcp_accept_socket,
		tcp_active_echo,
		tcp_echo,
		tcp_max_connections,
		tcp_max_connections_and_beyond,
		tcp_upgrade
	]}, {ssl, [
		ssl_accept_error,
		ssl_accept_socket,
		ssl_active_echo,
		ssl_echo
	]}, {misc, [
		misc_bad_transport
	]}, {supervisor, [
		supervisor_clean_restart,
		supervisor_clean_child_restart,
		supervisor_conns_alive
	]}].

init_per_suite(Config) ->
	ok = application:start(ranch),
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

%% misc.

misc_bad_transport(_) ->
	{error, badarg} = ranch:start_listener(misc_bad_transport, 1,
		bad_transport, [{port, 0}],
		echo_protocol, []),
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

ssl_accept_socket(Config) ->
	%%% XXX we can't do the spawn to test the controlling process change
	%%% because of the bug in ssl
	{ok, S} = ssl:listen(0,
		[{certfile, ?config(data_dir, Config) ++ "cert.pem"}, binary,
			{active, false}, {packet, raw}, {reuseaddr, true}]),
	{ok, _} = ranch:start_listener(ssl_accept_socket, 1,
		ranch_ssl, [{socket, S}], echo_protocol, []),
	Port = ranch:get_port(ssl_accept_socket),
	{ok, Socket} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw},
		{certfile, ?config(data_dir, Config) ++ "cert.pem"}]),
	ok = ssl:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(ssl_accept_socket),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(ssl_accept_socket) end,
	ok.

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

tcp_accept_socket(_) ->
	Ref = make_ref(),
	Parent = self(),
	spawn(fun() ->
				{ok, S} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw},
						{reuseaddr, true}]),
				{ok, _} = ranch:start_listener(tcp_accept_socket, 1,
					ranch_tcp, [{socket, S}], echo_protocol, []),
				Parent ! Ref
		end),
	receive
		Ref -> ok
	end,

	Port = ranch:get_port(tcp_accept_socket),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(tcp_accept_socket),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(tcp_accept_socket) end,
	ok.

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

%% Supervisor tests

supervisor_clean_restart(_) ->
	%% There we verify that mature listener death will not let
	%% whole supervisor down and also the supervisor itself will
	%% restart everything properly.
	Ref = supervisor_clean_restart,
	NbAcc = 4,
	{ok, Pid} = ranch:start_listener(Ref,
		NbAcc, ranch_tcp, [{port, 0}], echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ListenerPid0 = ranch_server:lookup_listener(Ref),
	erlang:exit(ListenerPid0, kill),
	receive after 1000 -> ok end,
	%% Verify that supervisor is alive
	true = is_process_alive(Pid),
	%% ...but children are dead.
	false = is_process_alive(ListenerPid0),
	%% Receive traces from newly started children
	ListenerPid = receive {trace, Pid, spawn, Pid1, _} -> Pid1 end,
	_ConnSupPid = receive {trace, Pid, spawn, Pid2, _} -> Pid2 end,
	AccSupPid = receive {trace, Pid, spawn, Pid3, _} -> Pid3 end,
	%% ...and its acceptors.
	[receive {trace, AccSupPid, spawn, _Pid, _} -> ok end ||
		_ <- lists:seq(1, NbAcc)],
	%% No more traces then.
	receive
		{trace, EPid, spawn, _, _} when EPid == Pid; EPid == AccSupPid ->
			error(invalid_restart)
	after 1000 -> ok end,
	%% Verify that new children registered themselves properly.
	ListenerPid = ranch_server:lookup_listener(Ref),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces().

supervisor_clean_child_restart(_) ->
	%% Then we verify that only parts of the supervision tree
	%% restarted in the case of failure.
	Ref = supervisor_clean_child_restart,
	%% Trace socket allocations.
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1}, [{'_', [], [{return_trace}]}], [global]),
	{ok, Pid} = ranch:start_listener(Ref,
		1, ranch_tcp, [{port, 0}], echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ListenerPid0 = ranch_server:lookup_listener(Ref),
	%% Manually shut the listening socket down.
	LSocket = receive
		{trace, _, return_from, {ranch_tcp, listen, 1}, {ok, Socket}} ->
			Socket
	after 0 ->
		error(lsocket_unknown)
	end,
	ok = gen_tcp:close(LSocket),
	receive after 1000 -> ok end,
	%% Verify that supervisor and its first two children are alive.
	true = is_process_alive(Pid),
	true = is_process_alive(ListenerPid0),
	%% Check that acceptors_sup is restarted properly.
	AccSupPid = receive {trace, Pid, spawn, Pid1, _} -> Pid1 end,
	AccPid = receive {trace, AccSupPid, spawn, Pid2, _} -> Pid2 end,
	receive {trace, AccPid, spawn, _, _} -> ok end,
	%% No more traces then.
	receive
		{trace, _, spawn, _, _} -> error(invalid_restart)
	after 1000 -> ok end,
	%% Verify that children still registered right.
	ListenerPid0 = ranch_server:lookup_listener(Ref),
	_ = erlang:trace_pattern({ranch_tcp, listen, 1}, false, []),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok.

supervisor_conns_alive(_) ->
	%% And finally we make sure that in the case of partial failure
	%% live connections are not being killed.
	Ref = supervisor_conns_alive,
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1}, [{'_', [], [{return_trace}]}], [global]),
	{ok, _} = ranch:start_listener(Ref,
		1, ranch_tcp, [{port, 0}], remove_conn_and_wait_protocol, [{remove, false}]),
	ok,
	%% Get the listener socket
	LSocket = receive
		{trace, _, return_from, {ranch_tcp, listen, 1}, {ok, S}} ->
			S
	after 0 ->
		error(lsocket_unknown)
	end,
	TcpPort = ranch:get_port(Ref),
	{ok, Socket} = gen_tcp:connect("localhost", TcpPort,
		[binary, {active, true}, {packet, raw}]),
	%% Shut the socket down
	ok = gen_tcp:close(LSocket),
	%% Assert that client is still viable.
	receive {tcp_closed, _} -> error(closed) after 1500 -> ok end,
	ok = gen_tcp:send(Socket, <<"poke">>),
	receive {tcp_closed, _} -> ok end,
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces().

clean_traces() ->
	receive
		{trace, _, _, _} ->
			clean_traces();
		{trace, _, _, _, _} ->
			clean_traces()
	after 0 ->
		ok
	end.

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
