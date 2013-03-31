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
-export([tcp_set_max_connections/1]).
-export([tcp_infinity_max_connections/1]).
-export([tcp_upgrade/1]).

%% supervisor.
-export([supervisor_clean_restart/1]).
-export([supervisor_clean_child_restart/1]).
-export([supervisor_conns_alive/1]).
-export([supervisor_listener_recover_state/1]).

%% ct.

all() ->
	[{group, tcp}, {group, ssl}, {group, misc}, {group, supervisor}].

groups() ->
	[{tcp, [
		tcp_accept_socket,
		tcp_active_echo,
		tcp_echo,
		tcp_max_connections,
		tcp_infinity_max_connections,
		tcp_max_connections_and_beyond,
		tcp_set_max_connections,
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
		supervisor_conns_alive,
		supervisor_listener_recover_state
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
	Name = ssl_accept_error,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_ssl, [{port, 0},
			{certfile, ?config(data_dir, Config) ++ "cert.pem"}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	[AcceptorPid] = ets:lookup_element(ranch_server,
		{acceptors, Name}, 2),
	true = is_process_alive(AcceptorPid),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:close(Socket),
	receive after 500 -> ok end,
	true = is_process_alive(AcceptorPid),
	ranch:stop_listener(Name).

ssl_accept_socket(Config) ->
	%%% XXX we can't do the spawn to test the controlling process change
	%%% because of the bug in ssl
	Name = ssl_accept_socket,
	{ok, S} = ssl:listen(0,
		[{certfile, ?config(data_dir, Config) ++ "cert.pem"}, binary,
			{active, false}, {packet, raw}, {reuseaddr, true}]),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_ssl, [{socket, S}], echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw},
		{certfile, ?config(data_dir, Config) ++ "cert.pem"}]),
	ok = ssl:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_active_echo(Config) ->
	Name = ssl_active_echo,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_ssl, [{port, 0},
			{certfile, ?config(data_dir, Config) ++ "cert.pem"}],
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw},
		{certfile, ?config(data_dir, Config) ++ "cert.pem"}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_echo(Config) ->
	Name = ssl_echo,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_ssl, [{port, 0},
			{certfile, ?config(data_dir, Config) ++ "cert.pem"}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw},
		{certfile, ?config(data_dir, Config) ++ "cert.pem"}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

%% tcp.

tcp_accept_socket(_) ->
	Name = tcp_accept_socket,
	Ref = make_ref(),
	Parent = self(),
	spawn(fun() ->
		{ok, S} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw},
			{reuseaddr, true}]),
		{ok, _} = ranch:start_listener(Name, 1,
			ranch_tcp, [{socket, S}], echo_protocol, []),
		Parent ! Ref
	end),
	receive
		Ref -> ok
	end,
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_active_echo(_) ->
	Name = tcp_active_echo,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}], active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_echo(_) ->
	Name = tcp_echo,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}], echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_max_connections(_) ->
	Name = tcp_max_connections,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {max_connections, 10}],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 11, 150),
	10 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 400),
	1 = receive_loop(connected, 1000),
	ranch:stop_listener(Name).

tcp_max_connections_and_beyond(_) ->
	Name = tcp_max_connections_and_beyond,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {max_connections, 10}],
		remove_conn_and_wait_protocol, [{remove, true}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	0 = ranch_server:count_connections(Name),
	10 = length(supervisor:which_children(
		ranch_server:lookup_connections_sup(Name))),
	Counts = supervisor:count_children(
		ranch_server:lookup_connections_sup(Name)),
	{_, 1} = lists:keyfind(specs, 1, Counts),
	{_, 0} = lists:keyfind(supervisors, 1, Counts),
	{_, 10} = lists:keyfind(active, 1, Counts),
	{_, 10} = lists:keyfind(workers, 1, Counts),
	ranch:set_protocol_options(Name, [{remove, false}]),
	receive after 250 -> ok end,
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	10 = ranch_server:count_connections(Name),
	20 = length(supervisor:which_children(
		ranch_server:lookup_connections_sup(Name))),
	Counts2 = supervisor:count_children(
		ranch_server:lookup_connections_sup(Name)),
	{_, 20} = lists:keyfind(active, 1, Counts2),
	{_, 20} = lists:keyfind(workers, 1, Counts2),
	ranch:stop_listener(Name).

tcp_set_max_connections(_) ->
	Name = tcp_set_max_connections,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {max_connections, 10}],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 20, 0),
	10 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 1000),
	10 = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, 20),
	10 = receive_loop(connected, 1000),
	20 = ranch:get_max_connections(Name),
	ranch:stop_listener(Name).

tcp_infinity_max_connections(_) ->
	Name = tcp_infinity_max_connections,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {max_connections, 10}],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 20, 0),
	10 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 1000),
	10 = ranch_server:count_connections(Name),
	10 = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, infinity),
	receive after 250 -> ok end,
	20 = ranch_server:count_connections(Name),
	infinity = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, 10),
	20 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 1000),
	ranch:stop_listener(Name).

tcp_upgrade(_) ->
	Name = tcp_upgrade,
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 1, 0),
	receive connected -> ok after 1000 -> error(timeout) end,
	ranch:set_protocol_options(Name, [{msg, upgraded}, {pid, self()}]),
	ok = connect_loop(Port, 1, 0),
	receive upgraded -> ok after 1000 -> error(timeout) end,
	ranch:stop_listener(Name).

%% Supervisor tests

supervisor_clean_restart(_) ->
	%% There we verify that mature listener death will not let
	%% whole supervisor down and also the supervisor itself will
	%% restart everything properly.
	Name = supervisor_clean_restart,
	NbAcc = 4,
	{ok, Pid} = ranch:start_listener(Name,
		NbAcc, ranch_tcp, [{port, 0}], echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ListenerPid0 = ranch_server:lookup_listener(Name),
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
	ListenerPid = ranch_server:lookup_listener(Name),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ranch:stop_listener(Name).

supervisor_clean_child_restart(_) ->
	%% Then we verify that only parts of the supervision tree
	%% restarted in the case of failure.
	Name = supervisor_clean_child_restart,
	%% Trace socket allocations.
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, Pid} = ranch:start_listener(Name,
		1, ranch_tcp, [{port, 0}], echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ListenerPid = ranch_server:lookup_listener(Name),
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
	true = is_process_alive(ListenerPid),
	%% Check that acceptors_sup is restarted properly.
	AccSupPid = receive {trace, Pid, spawn, Pid1, _} -> Pid1 end,
	receive {trace, AccSupPid, spawn, _, _} -> ok end,
	%% No more traces then.
	receive
		{trace, _, spawn, _, _} -> error(invalid_restart)
	after 1000 -> ok end,
	%% Verify that children still registered right.
	ListenerPid = ranch_server:lookup_listener(Name),
	_ = erlang:trace_pattern({ranch_tcp, listen, 1}, false, []),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ranch:stop_listener(Name).

supervisor_conns_alive(_) ->
	%% And finally we make sure that in the case of partial failure
	%% live connections are not being killed.
	Name = supervisor_conns_alive,
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}],
		remove_conn_and_wait_protocol, [{remove, false}]),
	%% Get the listener socket
	LSocket = receive
		{trace, _, return_from, {ranch_tcp, listen, 1}, {ok, S}} ->
			S
	after 0 ->
		error(lsocket_unknown)
	end,
	TcpPort = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", TcpPort,
		[binary, {active, true}, {packet, raw}]),
	%% Shut the socket down
	ok = gen_tcp:close(LSocket),
	%% Assert that client is still viable.
	receive {tcp_closed, _} -> error(closed) after 1500 -> ok end,
	ok = gen_tcp:send(Socket, <<"poke">>),
	receive {tcp_closed, _} -> ok end,
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ranch:stop_listener(Name).

supervisor_listener_recover_state(_) ->
	Name = supervisor_listener_recover_state,
	{ok, _} = ranch:start_listener(Name, 1,
			ranch_tcp, [{port, 0}, {max_connections, 10}], echo_protocol, []),
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_listener, init, 1},
			[{'_', [], [{return_trace}]}], [global]),
	ok = ranch:set_max_connections(Name, 20),
	ok = ranch:set_protocol_options(Name, [new]),
	ListenerPid = ranch_server:lookup_listener(Name),
	erlang:exit(ListenerPid, kill),
	receive
		{trace, _, return_from, {ranch_listener, init, 1}, _Result} ->
			ok
	after
		1000 ->
			error(timeout)
	end,
	%% check state was recovered
	20 = ranch:get_max_connections(Name),
	[new] = ranch:get_protocol_options(Name),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ranch:stop_listener(Name).

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

clean_traces() ->
	receive
		{trace, _, _, _} ->
			clean_traces();
		{trace, _, _, _, _} ->
			clean_traces()
	after 0 ->
		ok
	end.
