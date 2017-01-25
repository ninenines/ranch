%% Copyright (c) 2011-2016, Loïc Hoguin <essen@ninenines.eu>
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
-compile(export_all).

-import(ct_helper, [doc/1]).
-import(ct_helper, [name/0]).

%% ct.

all() ->
	[{group, tcp}, {group, ssl}, {group, misc}, {group, supervisor}].

groups() ->
	[{tcp, [
		tcp_accept_socket,
		tcp_active_echo,
		tcp_echo,
		tcp_inherit_options,
		tcp_max_connections,
		tcp_max_connections_and_beyond,
		tcp_max_connections_infinity,
		tcp_remove_connections,
		tcp_set_max_connections,
		tcp_set_max_connections_clean,
		tcp_upgrade,
		tcp_error_eaddrinuse,
		tcp_error_eacces
	]}, {ssl, [
		ssl_accept_error,
		ssl_accept_socket,
		ssl_active_echo,
		ssl_echo,
		ssl_sni_echo,
		ssl_sni_fail,
		ssl_error_eaddrinuse,
		ssl_error_no_cert,
		ssl_error_eacces
	]}, {misc, [
		misc_bad_transport,
		misc_bad_transport_options,
		misc_info
	]}, {supervisor, [
		connection_type_supervisor,
		connection_type_supervisor_separate_from_connection,
		supervisor_clean_child_restart,
		supervisor_clean_conns_sup_restart,
		supervisor_clean_restart,
		supervisor_conns_alive,
		supervisor_protocol_start_link_crash,
		supervisor_server_recover_state
	]}].

%% misc.

misc_bad_transport(_) ->
	doc("Reject invalid transport modules."),
	{error, badarg} = ranch:start_listener(misc_bad_transport, 1,
		bad_transport, [], echo_protocol, []),
	ok.

misc_bad_transport_options(_) ->
	doc("Ignore invalid transport options."),
	{ok, _} = ranch:start_listener(misc_bad_transport, 1,
		ranch_tcp, [binary, {packet, 4}, <<"garbage">>, raw, backlog], echo_protocol, []),
	ok.

misc_info(_) ->
	doc("Information about listeners."),
	%% Open a listener with a few connections.
	{ok, Pid1} = ranch:start_listener({misc_info, tcp}, 1, ranch_tcp, [],
		remove_conn_and_wait_protocol, [{remove, true, 2500}]),
	Port1 = ranch:get_port({misc_info, tcp}),
	%% Open a few more listeners with different arguments.
	{ok, Pid2} = ranch:start_listener({misc_info, act}, 2, ranch_tcp, [], active_echo_protocol, {}),
	Port2 = ranch:get_port({misc_info, act}),
	ranch:set_max_connections({misc_info, act}, infinity),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, Pid3} = ranch:start_listener({misc_info, ssl}, 3, ranch_ssl, Opts, echo_protocol, [{}]),
	Port3 = ranch:get_port({misc_info, ssl}),
	%% Open 5 connections, 3 removed from the count.
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	receive after 250 -> ok end,
	ranch:set_protocol_options({misc_info, tcp}, [{remove, false, 2500}]),
	receive after 250 -> ok end,
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	receive after 250 -> ok end,
	%% Confirm the info returned by Ranch is correct.
	[
		{{misc_info, act}, [
			{pid, Pid2},
			{ip, _},
			{port, Port2},
			{num_acceptors, 2},
			{max_connections, infinity}, %% Option was modified.
			{active_connections, 0},
			{all_connections, 0},
			{transport, ranch_tcp},
			{transport_options, []},
			{protocol, active_echo_protocol},
			{protocol_options, {}}
		]},
		{{misc_info, ssl}, [
			{pid, Pid3},
			{ip, _},
			{port, Port3},
			{num_acceptors, 3},
			{max_connections, 1024},
			{active_connections, 0},
			{all_connections, 0},
			{transport, ranch_ssl},
			{transport_options, Opts},
			{protocol, echo_protocol},
			{protocol_options, [{}]}
		]},
		{{misc_info, tcp}, [
			{pid, Pid1},
			{ip, _},
			{port, Port1},
			{num_acceptors, 1},
			{max_connections, 1024},
			{active_connections, 2},
			{all_connections, 5},
			{transport, ranch_tcp},
			{transport_options, []},
			{protocol, remove_conn_and_wait_protocol},
			{protocol_options, [{remove, false, 2500}]} %% Option was modified.
		]}
	] = lists:sort([L || L={{misc_info, _}, _} <- ranch:info()]),
	%% Get acceptors.
	[_] = ranch:procs({misc_info, tcp}, acceptors),
	[_, _] = ranch:procs({misc_info, act}, acceptors),
	[_, _, _] = ranch:procs({misc_info, ssl}, acceptors),
	%% Get connections.
	[_, _, _, _, _] = ranch:procs({misc_info, tcp}, connections),
	[] = ranch:procs({misc_info, act}, connections),
	[] = ranch:procs({misc_info, ssl}, connections),
	ok.

%% ssl.

ssl_accept_error(_) ->
	doc("Acceptor must not crash if client disconnects in the middle of SSL handshake."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, ListenerSup} = ranch:start_listener(Name, 1, ranch_ssl, Opts, echo_protocol, []),
	Port = ranch:get_port(Name),
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, AcceptorsSup, _, _} = lists:keyfind(ranch_acceptors_sup, 1, ListenerSupChildren),
	[{{acceptor, _, _}, AcceptorPid, _, _}] = supervisor:which_children(AcceptorsSup),
	true = is_process_alive(AcceptorPid),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:close(Socket),
	receive after 500 -> ok end,
	true = is_process_alive(AcceptorPid),
	ok = ranch:stop_listener(Name).

ssl_accept_socket(_) ->
	doc("Ensure that listener can use an externally opened SSL listen socket."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, S} = ssl:listen(0, [binary, {active, false}, {packet, raw}, {reuseaddr, true}|Opts]),
	{ok, _} = ranch:start_listener(Name, 1, ranch_ssl, [{socket, S}], echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_active_echo(_) ->
	doc("Ensure that active mode works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_ssl, Opts, active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_echo(_) ->
	doc("Ensure that passive mode works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_ssl, Opts, echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_sni_echo(_) ->
	case application:get_key(ssl, vsn) of
		{ok, Vsn} when Vsn >= "7.0" ->
			do_ssl_sni_echo();
		_ ->
			{skip, "No SNI support."}
	end.

do_ssl_sni_echo() ->
	doc("Ensure that SNI works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_ssl, [{sni_hosts, [{"localhost", Opts}]}], echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_sni_fail(_) ->
	case application:get_key(ssl, vsn) of
		{ok, Vsn} when Vsn >= "7.0" ->
			do_ssl_sni_fail();
		_ ->
			{skip, "No SNI support."}
	end.

do_ssl_sni_fail() ->
	doc("Ensure that connection fails when host is not in SNI list."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_ssl, [{sni_hosts, [{"pouet", Opts}]}], echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, _} = ssl:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_error_eaddrinuse(_) ->
	doc("Ensure that failure due to an eaddrinuse returns a compact readable error."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_ssl, Opts, active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, eaddrinuse} = ranch:start_listener({Name, fails}, 1,
		ranch_ssl, [{port, Port}|Opts], active_echo_protocol, []),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_error_no_cert(_) ->
	doc("Ensure that failure due to missing certificate returns a compact readable error."),
	{error, no_cert} = ranch:start_listener(name(), 1, ranch_ssl, [], active_echo_protocol, []),
	ok.

ssl_error_eacces(_) ->
	doc("Ensure that failure due to an eacces returns a compact readable error."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{error, eacces} = ranch:start_listener(Name, 1,
		ranch_ssl, [{port, 283}|Opts], active_echo_protocol, []),
	ok.

%% tcp.

tcp_accept_socket(_) ->
	doc("Ensure that listener can use an externally opened TCP listen socket."),
	Name = name(),
	{ok, S} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw}, {reuseaddr, true}]),
	{ok, _} = ranch:start_listener(Name, 1, ranch_tcp, [{socket, S}], echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_active_echo(_) ->
	doc("Ensure that active mode works with TCP transport."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_tcp, [], active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_echo(_) ->
	doc("Ensure that passive mode works with TCP transport."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_tcp, [], echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_inherit_options(_) ->
	doc("Ensure TCP options are inherited in the protocol."),
	Name = name(),
	Opts = [{nodelay, false}, {send_timeout_close, false}],
	{ok, _} = ranch:start_listener(Name, 4, ranch_tcp, Opts, check_tcp_options, [{pid, self()} | Opts]),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, raw}]),
	receive checked -> ok after 1000 -> error(timeout) end,
	ok = gen_tcp:close(Socket),
	ok = ranch:stop_listener(Name).

tcp_max_connections(_) ->
	doc("Ensure the max_connections option actually limits connections."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{max_connections, 10}],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 11, 150),
	10 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 400),
	1 = receive_loop(connected, 1000),
	ok = ranch:stop_listener(Name).

tcp_max_connections_and_beyond(_) ->
	doc("Ensure the max_connections option works when connections are removed from the count."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{max_connections, 10}],
		remove_conn_and_wait_protocol, [{remove, true, 2500}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	0 = ranch_server:count_connections(Name),
	10 = length(supervisor:which_children(ranch_server:get_connections_sup(Name))),
	Counts = supervisor:count_children(ranch_server:get_connections_sup(Name)),
	{_, 1} = lists:keyfind(specs, 1, Counts),
	{_, 0} = lists:keyfind(supervisors, 1, Counts),
	{_, 10} = lists:keyfind(active, 1, Counts),
	{_, 10} = lists:keyfind(workers, 1, Counts),
	ranch:set_protocol_options(Name, [{remove, false, 2500}]),
	receive after 250 -> ok end,
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	10 = ranch_server:count_connections(Name),
	20 = length(supervisor:which_children(ranch_server:get_connections_sup(Name))),
	Counts2 = supervisor:count_children(ranch_server:get_connections_sup(Name)),
	{_, 20} = lists:keyfind(active, 1, Counts2),
	{_, 20} = lists:keyfind(workers, 1, Counts2),
	ok = ranch:stop_listener(Name).

tcp_max_connections_infinity(_) ->
	doc("Set the max_connections option from 10 to infinity and back to 10."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{max_connections, 10}],
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
	ok = ranch:stop_listener(Name).

tcp_remove_connections(_) ->
	doc("Ensure that removed connections are only removed once."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [],
		remove_conn_and_wait_protocol, [{remove, true, 0}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	0 = ranch_server:count_connections(Name),
	ok = ranch:stop_listener(Name).

tcp_set_max_connections(_) ->
	doc("Ensure that changing the max_connections option to a larger value allows for more connections."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{max_connections, 10}],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 20, 0),
	10 = ranch_server:count_connections(Name),
	10 = receive_loop(connected, 1000),
	10 = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, 20),
	10 = receive_loop(connected, 1000),
	20 = ranch:get_max_connections(Name),
	ok = ranch:stop_listener(Name).

tcp_set_max_connections_clean(_) ->
	doc("Ensure that setting max_connections does not crash any process."),
	Name = name(),
	{ok, ListSupPid} = ranch:start_listener(Name, 4, ranch_tcp,
			[{max_connections, 4}],
			notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Children = supervisor:which_children(ListSupPid),
	{_, AccSupPid, _, _} = lists:keyfind(ranch_acceptors_sup, 1, Children),
	1 = erlang:trace(ListSupPid, true, [procs]),
	1 = erlang:trace(AccSupPid, true, [procs]),
	Port = ranch:get_port(Name),
	N = 20,
	ok = connect_loop(Port, N*5, 0),
	%% Randomly set max_connections.
	[spawn(ranch, set_max_connections, [Name, Max]) ||
		Max <- lists:flatten(lists:duplicate(N, [6, 4, 8, infinity]))],
	receive
		{trace, _, spawn, _, _} ->
			error(dirty_set_max_connections)
	after
		2000 -> ok
	end,
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

tcp_upgrade(_) ->
	doc("Ensure that protocol options can be updated."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [],
		notify_and_wait_protocol, [{msg, connected}, {pid, self()}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 1, 0),
	receive connected -> ok after 1000 -> error(timeout) end,
	ranch:set_protocol_options(Name, [{msg, upgraded}, {pid, self()}]),
	ok = connect_loop(Port, 1, 0),
	receive upgraded -> ok after 1000 -> error(timeout) end,
	ok = ranch:stop_listener(Name).

tcp_error_eaddrinuse(_) ->
	doc("Ensure that failure due to an eaddrinuse returns a compact readable error."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_tcp, [], active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, eaddrinuse} = ranch:start_listener({Name, fails}, 1,
		ranch_tcp, [{port, Port}], active_echo_protocol, []),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_error_eacces(_) ->
	doc("Ensure that failure due to an eacces returns a compact readable error."),
	Name = name(),
	{error, eacces} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 283}], active_echo_protocol, []),
	ok.


%% Supervisor tests

connection_type_supervisor(_) ->
	doc("The supervisor connection type must be reflected in the specifications."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{connection_type, supervisor}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ConnsSup = ranch_server:get_connections_sup(Name),
	[{echo_protocol, _, supervisor, [echo_protocol]}] = supervisor:which_children(ConnsSup),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

connection_type_supervisor_separate_from_connection(_) ->
	doc("The supervisor connection type allows separate supervised and connection processes."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [{connection_type, supervisor}],
		supervisor_separate, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ConnsSup = ranch_server:get_connections_sup(Name),
	[{supervisor_separate, _, supervisor, [supervisor_separate]}] = supervisor:which_children(ConnsSup),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

supervisor_clean_child_restart(_) ->
	doc("Verify that only the relevant parts of the supervision tree restarted "
		"when the listening socket is closed."),
	Name = name(),
	%% Trace socket allocations.
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, Pid} = ranch:start_listener(Name,
		1, ranch_tcp, [], echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ConnsSup = ranch_server:get_connections_sup(Name),
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
	true = is_process_alive(ConnsSup),
	%% Check that acceptors_sup is restarted properly.
	AccSupPid = receive {trace, Pid, spawn, Pid1, _} -> Pid1 end,
	receive {trace, AccSupPid, spawn, _, _} -> ok end,
	%% No more traces then.
	receive
		{trace, _, spawn, _, _} -> error(invalid_restart)
	after 1000 -> ok end,
	%% Verify that children still registered right.
	ConnsSup = ranch_server:get_connections_sup(Name),
	_ = erlang:trace_pattern({ranch_tcp, listen, 1}, false, []),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

supervisor_clean_conns_sup_restart(_) ->
	doc("Verify that a conns_sup can not register with the same name as an already "
		"registered ranch_conns_sup that is still alive. Make sure this does not crash "
		"the ranch_server process."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		1, ranch_tcp, [], echo_protocol, []),
	Server = erlang:whereis(ranch_server),
	ServerMonRef = erlang:monitor(process, Server),
	%% Exit because Name already registered and is alive.
	{'EXIT', _}  = (catch ranch_server:set_connections_sup(Name, self())),
	receive
		{'DOWN', ServerMonRef, process, Server, _} ->
			error(ranch_server_down)
	after
		1000 ->
			ok
	end,
	ok = ranch:stop_listener(Name).

supervisor_clean_restart(_) ->
	doc("Verify that killing ranch_conns_sup does not crash everything "
		"and that it restarts properly."),
	Name = name(),
	NumAcc = 4,
	{ok, Pid} = ranch:start_listener(Name, NumAcc, ranch_tcp, [], echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ConnsSup0 = ranch_server:get_connections_sup(Name),
	erlang:exit(ConnsSup0, kill),
	receive after 1000 -> ok end,
	%% Verify that supervisor is alive
	true = is_process_alive(Pid),
	%% ...but children are dead.
	false = is_process_alive(ConnsSup0),
	%% Receive traces from newly started children
	ConnsSup = receive {trace, Pid, spawn, Pid2, _} -> Pid2 end,
	AccSupPid = receive {trace, Pid, spawn, Pid3, _} -> Pid3 end,
	%% ...and its acceptors.
	[receive {trace, AccSupPid, spawn, _Pid, _} -> ok end ||
		_ <- lists:seq(1, NumAcc)],
	%% No more traces then.
	receive
		{trace, EPid, spawn, _, _} when EPid == Pid; EPid == AccSupPid ->
			error(invalid_restart)
	after 1000 -> ok end,
	%% Verify that new children registered themselves properly.
	ConnsSup = ranch_server:get_connections_sup(Name),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

supervisor_conns_alive(_) ->
	doc("Ensure that active connections stay open when the listening socket gets closed."),
	Name = name(),
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, _} = ranch:start_listener(Name, 1,
		ranch_tcp, [],
		remove_conn_and_wait_protocol, [{remove, false, 2500}]),
	%% Get the listener socket
	LSocket = receive
		{trace, _, return_from, {ranch_tcp, listen, 1}, {ok, S}} ->
			S
	after 500 ->
		error(lsocket_unknown)
	end,
	TcpPort = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", TcpPort,
		[binary, {active, true}, {packet, raw}]),
	receive after 500 -> ok end,
	%% Shut the socket down
	ok = gen_tcp:close(LSocket),
	%% Assert that client is still viable.
	receive {tcp_closed, _} -> error(closed) after 1500 -> ok end,
	ok = gen_tcp:send(Socket, <<"poke">>),
	receive {tcp_closed, _} -> ok end,
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

supervisor_protocol_start_link_crash(_) ->
	doc("Ensure a protocol start crash does not kill all connections."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, 1, ranch_tcp, [], crash_protocol, []),
	ConnsSup = ranch_server:get_connections_sup(Name),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, raw}]),
	receive after 500 -> ok end,
	ConnsSup = ranch_server:get_connections_sup(Name),
	ok = ranch:stop_listener(Name).

supervisor_server_recover_state(_) ->
	doc("Ensure that when ranch_server crashes and restarts, it recovers "
		"its state and continues monitoring the same processes."),
	Name = name(),
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_server, init, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, _} = ranch:start_listener(Name, 1, ranch_tcp, [], echo_protocol, []),
	ConnsSup = ranch_server:get_connections_sup(Name),
	ServerPid = erlang:whereis(ranch_server),
	{monitors, Monitors} = erlang:process_info(ServerPid, monitors),
	erlang:exit(ServerPid, kill),
	receive
		{trace, ServerPid2, return_from, {ranch_server, init, 1}, _Result} ->
			{monitors, Monitors2} = erlang:process_info(ServerPid2, monitors),
			%% Check that ranch_server is monitoring the same processes.
			true = (lists:usort(Monitors) == lists:usort(Monitors2))
	after
		1000 ->
			error(timeout)
	end,
	ConnsSup = ranch_server:get_connections_sup(Name),
	ok = ranch:stop_listener(Name),
	%% Check ranch_server has removed the ranch_conns_sup.
	{'EXIT', {badarg, _}} = (catch ranch_server:get_connections_sup(Name)),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces().

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
