%% Copyright (c) 2011-2021, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2020-2021, Jan Uhlig <juhlig@hnc-agency.org>
%% Copyright (c) 2021, Maria Scott <maria-12648430@hnc-agency.org>
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
-compile(nowarn_export_all).

-dialyzer({nowarn_function, misc_wait_for_connections/1}).
%% @todo Remove when specs in ssl are updated to accept local addresses.
-dialyzer({nowarn_function, do_ssl_local_echo/0}).

-import(ct_helper, [config/2]).
-import(ct_helper, [doc/1]).
-import(ct_helper, [name/0]).

%% ct.

all() ->
	[{group, tcp}, {group, tcp_socket}, {group, ssl}, {group, misc}, {group, supervisor}].

groups() ->
	[{tcp, [
		tcp_active_echo,
		tcp_active_n_echo,
		tcp_echo,
		tcp_local_echo,
		tcp_graceful,
		tcp_inherit_options,
		tcp_max_connections,
		tcp_max_connections_and_beyond,
		tcp_max_connections_infinity,
		tcp_remove_connections,
		tcp_remove_connections_acceptor_wakeup,
		tcp_set_max_connections,
		tcp_set_max_connections_clean,
		tcp_getopts_capability,
		tcp_getstat_capability,
		tcp_upgrade,
		tcp_10_acceptors_10_listen_sockets,
		tcp_many_listen_sockets_no_reuseport,
		tcp_error_eaddrinuse,
		tcp_error_eacces
	]}, {tcp_socket, [
		tcp_active_echo,
		tcp_active_n_echo,
		tcp_echo,
		tcp_graceful,
		tcp_inherit_options,
		tcp_max_connections,
		tcp_max_connections_and_beyond,
		tcp_max_connections_infinity,
		tcp_remove_connections,
		tcp_remove_connections_acceptor_wakeup,
		tcp_set_max_connections,
		tcp_set_max_connections_clean,
		tcp_getopts_capability,
		tcp_getstat_capability,
		tcp_upgrade,
		%% @TODO: Enable when https://github.com/erlang/otp/issues/5122
		%%        is in an official release, probably 24.1.
		% tcp_10_acceptors_10_listen_sockets,
		% tcp_many_listen_sockets_no_reuseport,
		tcp_error_eaddrinuse
		%% @TODO: Not working in OTP/24.0 but fixed in current master.
		%%        Enable when fixed in an official release, probably 24.1.
		% tcp_error_eacces
	]}, {ssl, [
		ssl_accept_error,
		ssl_active_echo,
		ssl_active_n_echo,
		ssl_echo,
		ssl_local_echo,
		ssl_graceful,
		ssl_handshake,
		ssl_handshake_error,
		ssl_sni_echo,
		ssl_sni_fail,
		ssl_tls_psk,
		ssl_tls_psk_fail,
		ssl_upgrade_from_tcp,
		ssl_getopts_capability,
		ssl_getstat_capability,
		ssl_10_acceptors_10_listen_sockets,
		ssl_many_listen_sockets_no_reuseport,
		ssl_error_eaddrinuse,
		ssl_error_no_cert,
		ssl_error_eacces,
		ssl_unsupported_tlsv13_options
	]}, {misc, [
		misc_bad_transport,
		misc_bad_transport_options,
		misc_repeated_remove,
		misc_info,
		misc_info_embedded,
		misc_metrics,
		misc_opts_logger,
		misc_post_listen_callback,
		misc_post_listen_callback_error,
		misc_set_transport_options,
		misc_wait_for_connections,
		misc_multiple_ip_local_socket_opts,
		misc_connection_alarms,
		misc_stop_unknown_listener
	]}, {supervisor, [
		connection_type_supervisor,
		connection_type_supervisor_separate_from_connection,
		supervisor_10_acceptors_1_conns_sup,
		supervisor_9_acceptors_4_conns_sups,
		supervisor_10_acceptors_10_conns_sups,
		supervisor_1_acceptor_10_conns_sups,
		supervisor_changed_options_restart,
		supervisor_clean_child_restart,
		supervisor_clean_restart,
		supervisor_conns_alive,
		supervisor_embedded_ranch_server_crash,
		supervisor_protocol_start_link_crash,
		supervisor_server_recover_state,
		supervisor_unexpected_message
	]}].

init_per_group(tcp_socket, Config) ->
	%% The socket backend for inet/gen_tcp was introduced as an experimental
	%% feature in OTP/23.0, and bugs https://bugs.erlang.org/browse/ERL-1284,
	%% 1287 and 1293 were solved in OTP/23.1. socket:use_registry/1 first
	%% appears in this release.
	%% Due to https://bugs.erlang.org/browse/ERL-1401, the socket backend
	%% is not working on Windows.
	case
		os:type() =/= {win32, nt} andalso
		code:ensure_loaded(socket) =:= {module, socket} andalso
		erlang:function_exported(socket, use_registry, 1)
	of
		true ->
			[{socket_opts, [{inet_backend, socket}]}|Config];
		false ->
			{skip, "No socket backend support"}
	end;
init_per_group(_, Config) ->
	case
		code:ensure_loaded(socket) =:= {module, socket} andalso
		erlang:function_exported(socket, use_registry, 1)
	of
		true ->
			[{socket_opts, [{inet_backend, inet}]}|Config];
		false ->
			[{socket_opts, []}|Config]
	end.

end_per_group(_, _) ->
	ok.

init_per_testcase(_, Config) ->
	Config.

end_per_testcase(_, _) ->
	%% Stop all listeners that a test case may have left running.
	_ = [catch ranch:stop_listener(Name) || Name <- maps:keys(ranch:info())],
	ok.

%% misc.

misc_bad_transport(_) ->
	doc("Reject invalid transport modules."),
	{error, {bad_transport, invalid_transport}} = ranch:start_listener(misc_bad_transport,
		invalid_transport, #{},
		echo_protocol, []),
	ok.

misc_bad_transport_options(_) ->
	doc("Ignore invalid transport options."),
	{ok, _} = ranch:start_listener(misc_bad_transport_options,
		ranch_tcp, [binary, {packet, 4}, <<"garbage">>, raw, backlog],
		echo_protocol, []),
	ok.

misc_info(_) ->
	doc("Information about listeners."),
	%% Open a listener with a few connections.
	{ok, Pid1} = ranch:start_listener({misc_info, tcp},
		ranch_tcp, #{num_acceptors => 1},
		remove_conn_and_wait_protocol, [{remove, true, 2500}]),
	Port1 = ranch:get_port({misc_info, tcp}),
	%% Open a few more listeners with different arguments.
	{ok, Pid2} = ranch:start_listener({misc_info, act},
		ranch_tcp, #{num_acceptors => 2},
		active_echo_protocol, {}),
	Port2 = ranch:get_port({misc_info, act}),
	ranch:set_max_connections({misc_info, act}, infinity),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, Pid3} = ranch:start_listener({misc_info, ssl},
		ranch_ssl, #{num_acceptors => 3, socket_opts => Opts},
		echo_protocol, [{}]),
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
	#{
		{misc_info, act} := #{
			pid := Pid2,
			port := Port2,
			max_connections := infinity, %% Option was modified.
			active_connections := 0,
			all_connections := 0,
			transport := ranch_tcp,
			transport_options := #{num_acceptors := 2},
			protocol := active_echo_protocol,
			protocol_options := {}
		},
		{misc_info, ssl} := #{
			pid := Pid3,
			port := Port3,
			max_connections := 1024,
			active_connections := 0,
			all_connections := 0,
			transport := ranch_ssl,
			transport_options := #{num_acceptors := 3, socket_opts := Opts},
			protocol := echo_protocol,
			protocol_options := [{}]
		},
		{misc_info, tcp} := #{
			pid := Pid1,
			port := Port1,
			max_connections := 1024,
			active_connections := 2,
			all_connections := 5,
			transport := ranch_tcp,
			transport_options := #{num_acceptors := 1},
			protocol := remove_conn_and_wait_protocol,
			protocol_options := [{remove, false, 2500}] %% Option was modified.
		}
	} = ranch:info(),
	%% Get acceptors.
	[_] = ranch:procs({misc_info, tcp}, acceptors),
	[_, _] = ranch:procs({misc_info, act}, acceptors),
	[_, _, _] = ranch:procs({misc_info, ssl}, acceptors),
	%% Get connections.
	[_, _, _, _, _] = ranch:procs({misc_info, tcp}, connections),
	[] = ranch:procs({misc_info, act}, connections),
	[] = ranch:procs({misc_info, ssl}, connections),
	ok.

misc_info_embedded(_) ->
	doc("Information about listeners in embedded mode."),
	{ok, SupPid} = embedded_sup:start_link(),
	%% Open a listener with a few connections.
	{ok, EmbeddedSupPid1} = embedded_sup:start_listener(SupPid, {misc_info_embedded, tcp},
		ranch_tcp, #{num_acceptors => 1},
		remove_conn_and_wait_protocol, [{remove, true, 2500}]),
	{_, Pid1, _, _} = lists:keyfind({ranch_listener_sup, {misc_info_embedded, tcp}}, 1,
		supervisor:which_children(EmbeddedSupPid1)),
	Port1 = ranch:get_port({misc_info_embedded, tcp}),
	%% Open a few more listeners with different arguments.
	{ok, EmbeddedSupPid2} = embedded_sup:start_listener(SupPid, {misc_info_embedded, act},
		ranch_tcp, #{num_acceptors => 2},
		active_echo_protocol, {}),
	{_, Pid2, _, _} = lists:keyfind({ranch_listener_sup, {misc_info_embedded, act}}, 1,
		supervisor:which_children(EmbeddedSupPid2)),
	Port2 = ranch:get_port({misc_info_embedded, act}),
	ranch:set_max_connections({misc_info_embedded, act}, infinity),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, EmbeddedSupPid3} = embedded_sup:start_listener(SupPid, {misc_info_embedded, ssl},
		ranch_ssl, #{num_acceptors => 3, socket_opts => Opts},
		echo_protocol, [{}]),
	{_, Pid3, _, _} = lists:keyfind({ranch_listener_sup, {misc_info_embedded, ssl}}, 1,
		supervisor:which_children(EmbeddedSupPid3)),
	Port3 = ranch:get_port({misc_info_embedded, ssl}),
	%% Open 5 connections, 3 removed from the count.
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	receive after 250 -> ok end,
	ranch:set_protocol_options({misc_info_embedded, tcp}, [{remove, false, 2500}]),
	receive after 250 -> ok end,
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	{ok, _} = gen_tcp:connect("localhost", Port1, [binary, {active, false}, {packet, raw}]),
	receive after 250 -> ok end,
	%% Confirm the info returned by Ranch is correct.
	#{
		{misc_info_embedded, act} := #{
			pid := Pid2,
			port := Port2,
			max_connections := infinity, %% Option was modified.
			active_connections := 0,
			all_connections := 0,
			transport := ranch_tcp,
			transport_options := #{num_acceptors := 2},
			protocol := active_echo_protocol,
			protocol_options := {}
		},
		{misc_info_embedded, ssl} := #{
			pid := Pid3,
			port := Port3,
			max_connections := 1024,
			active_connections := 0,
			all_connections := 0,
			transport := ranch_ssl,
			transport_options := #{num_acceptors := 3, socket_opts := Opts},
			protocol := echo_protocol,
			protocol_options := [{}]
		},
		{misc_info_embedded, tcp} := #{
			pid := Pid1,
			port := Port1,
			max_connections := 1024,
			active_connections := 2,
			all_connections := 5,
			transport := ranch_tcp,
			transport_options := #{num_acceptors := 1},
			protocol := remove_conn_and_wait_protocol,
			protocol_options := [{remove, false, 2500}] %% Option was modified.
		}
	} = ranch:info(),
	%% Get acceptors.
	[_] = ranch:procs({misc_info_embedded, tcp}, acceptors),
	[_, _] = ranch:procs({misc_info_embedded, act}, acceptors),
	[_, _, _] = ranch:procs({misc_info_embedded, ssl}, acceptors),
	%% Get connections.
	[_, _, _, _, _] = ranch:procs({misc_info_embedded, tcp}, connections),
	[] = ranch:procs({misc_info_embedded, act}, connections),
	[] = ranch:procs({misc_info_embedded, ssl}, connections),
	%% Stop embedded tcp listener and ensure it is gone.
	ok = embedded_sup:stop_listener(SupPid, {misc_info_embedded, tcp}),
	timer:sleep(500),
	false = maps:is_key({misc_info_embedded, tcp}, ranch:info()),
	%% Stop embedded act listener and ensure it is gone.
	ok = embedded_sup:stop_listener(SupPid, {misc_info_embedded, act}),
	timer:sleep(500),
	false = maps:is_key({misc_info_embedded, act}, ranch:info()),
	%% Stop embedded ssl listener and ensure it is gone.
	ok = embedded_sup:stop_listener(SupPid, {misc_info_embedded, ssl}),
	timer:sleep(500),
	false = maps:is_key({misc_info_embedded, ssl}, ranch:info()),
	%% Stop embedded supervisor.
	embedded_sup:stop(SupPid),
	ok.

misc_metrics(_) ->
	doc("Confirm accept/terminate metrics are correct."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name, ranch_tcp, #{},
		notify_and_wait_protocol, #{pid => self()}),
	Port = ranch:get_port(Name),
	%% Start 10 connections.
	ok = connect_loop(Port, 10, 0),
	{10, ConnPids1} = receive_loop(connected, 400),
	#{metrics := Metrics1} = ranch:info(Name),
	{10, 0} = do_accumulate_metrics(Metrics1),
	%% Start 10 more connections.
	ok = connect_loop(Port, 10, 0),
	{10, ConnPids2} = receive_loop(connected, 400),
	#{metrics := Metrics2} = ranch:info(Name),
	{20, 0} = do_accumulate_metrics(Metrics2),
	%% Terminate 10 connections.
	ok = terminate_loop(stop, ConnPids2),
	timer:sleep(100),
	#{metrics := Metrics3} = ranch:info(Name),
	{20, 10} = do_accumulate_metrics(Metrics3),
	%% Terminate 10 more connections.
	ok = terminate_loop(stop, ConnPids1),
	timer:sleep(100),
	#{metrics := Metrics4} = ranch:info(Name),
	{20, 20} = do_accumulate_metrics(Metrics4),
	ok = ranch:stop_listener(Name),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

do_accumulate_metrics(Metrics) ->
	maps:fold(
		fun
			({conns_sup, _, accept}, N, {Accepts, Terminates}) ->
				{Accepts+N, Terminates};
			({conns_sup, _, terminate}, N, {Accepts, Terminates}) ->
				{Accepts, Terminates+N}
		end,
		{0, 0},
		Metrics
	).

misc_opts_logger(_) ->
	doc("Confirm that messages are sent via the configured logger module."),
	register(misc_opts_logger, self()),
	{ok, _} = ranch:start_listener(name(),
		ranch_tcp, #{logger => ?MODULE, socket_opts => [<<"garbage">>]},
		echo_protocol, []),
	receive
		{warning, "Transport option " ++ _, [<<"garbage">>]} ->
			ok
	after 1000 ->
		error(timeout)
	end.

warning(Format, Args) ->
	misc_opts_logger ! {warning, Format, Args}.

misc_post_listen_callback(_) ->
	doc("Ensure that the post-listen callback works."),
	Name = name(),
	Self = self(),
	Ref = make_ref(),
	PostListenCb = fun (LSock) ->
		ok = ranch_tcp:setopts(LSock, [{send_timeout, 1000}]),
		Self ! {post_listen, Ref, LSock},
		ok
	end,
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{post_listen_callback => PostListenCb,
			socket_opts => [{send_timeout, infinity}]},
		echo_protocol, []),
	receive
		{post_listen, Ref, LSock} ->
			{ok, [{send_timeout, 1000}]} = ranch_tcp:getopts(LSock, [send_timeout]),
			ok
	after 1000 ->
		error(timeout)
	end,
	Port = ranch:get_port(Name),
	{ok, S} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(S, <<"Test">>),
	{ok, <<"Test">>} = gen_tcp:recv(S, 4, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(S, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

misc_post_listen_callback_error(_) ->
	doc("Ensure that starting a listener fails when the post-listen callback returns an error."),
	Name = name(),
	PostListenCb = fun (_) -> {error, test} end,
	{error, _} = ranch:start_listener(Name,
		ranch_tcp, #{post_listen_callback => PostListenCb},
		echo_protocol, []),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

misc_repeated_remove(_) ->
	doc("Ensure repeated removal of connection does not crash the connection supervisor."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		remove_conn_and_wait_protocol, [{remove, 5, 0}]),
	Port = ranch:get_port(Name),
	ConnsSups = lists:sort(ranch_server:get_connections_sups(Name)),
	{ok, _} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	timer:sleep(1000),
	ConnsSups = lists:sort(ranch_server:get_connections_sups(Name)),
	true = lists:all(fun ({_, ConnsSup}) -> erlang:is_process_alive(ConnsSup) end, ConnsSups),
	ok = ranch:stop_listener(Name).

misc_set_transport_options(_) ->
	doc(""),
	Name = name(),
	{ok, ListenerSupPid} = ranch:start_listener(Name, ranch_tcp, #{max_connections => 10,
		handshake_timeout => 5000, shutdown => 1000, num_acceptors => 1,
		socket_opts => [{send_timeout, 5000}]}, echo_protocol, []),
	ok = ranch:set_transport_options(Name, #{max_connections => 20, handshake_timeout => 5001,
		num_acceptors => 2, shutdown => 1001, socket_opts => [{send_timeout, 5002}]}),
	ConnsSups = [ConnsSup || {_, ConnsSup} <- ranch_server:get_connections_sups(Name)],
	_ = [begin
		{State, _, _, _} = sys:get_state(ConnsSup),
		20 = element(11, State),
		5001 = element(10, State),
		1001 = element(6, State)
	end || ConnsSup <- ConnsSups],
	ok = ranch:suspend_listener(Name),
	ok = ranch:resume_listener(Name),
	2 = length(ranch:procs(Name, acceptors)),
	LSocket = do_get_listener_socket(ListenerSupPid),
	{ok, [{send_timeout, 5002}]} = ranch_tcp:getopts(LSocket, [send_timeout]),
	ok = ranch:stop_listener(Name).

misc_wait_for_connections(_) ->
	doc("Ensure wait for connections works."),
	Name = name(),
	Self = self(),
	%% Ensure invalid arguments are rejected.
	{'EXIT', {badarg, _}} = begin catch ranch:wait_for_connections(Name, 'foo', 0) end,
	{'EXIT', {badarg, _}} = begin catch ranch:wait_for_connections(Name, '==', -1) end,
	{'EXIT', {badarg, _}} = begin catch ranch:wait_for_connections(Name, '==', 0, -1) end,
	{'EXIT', {badarg, _}} = begin catch ranch:wait_for_connections(Name, '<', 0) end,
	%% Create waiters for increasing number of connections.
	Pid1GT = do_create_waiter(Self, Name, '>', 0),
	Pid1GE = do_create_waiter(Self, Name, '>=', 1),
	Pid1EQ = do_create_waiter(Self, Name, '==', 1),
	Pid2GT = do_create_waiter(Self, Name, '>', 1),
	Pid2GE = do_create_waiter(Self, Name, '>=', 2),
	Pid2EQ = do_create_waiter(Self, Name, '==', 2),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{num_acceptors => 1},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	%% Create some connections, ensure that waiters respond.
	{ok, Sock1} = gen_tcp:connect("localhost", Port, []),
	ok = do_expect_waiter(Pid1GT),
	ok = do_expect_waiter(Pid1GE),
	ok = do_expect_waiter(Pid1EQ),
	ok = do_expect_waiter(undefined),
	{ok, Sock2} = gen_tcp:connect("localhost", Port, []),
	ok = do_expect_waiter(Pid2GT),
	ok = do_expect_waiter(Pid2GE),
	ok = do_expect_waiter(Pid2EQ),
	ok = do_expect_waiter(undefined),
	%% Create waiters for decreasing number of connections.
	Pid3LT = do_create_waiter(Self, Name, '<', 2),
	Pid3LE = do_create_waiter(Self, Name, '=<', 1),
	Pid3EQ = do_create_waiter(Self, Name, '==', 1),
	Pid4LT = do_create_waiter(Self, Name, '<', 1),
	Pid4LE = do_create_waiter(Self, Name, '=<', 0),
	Pid4EQ = do_create_waiter(Self, Name, '==', 0),
	%% Close connections, ensure that waiters respond.
	ok = gen_tcp:close(Sock1),
	ok = do_expect_waiter(Pid3LT),
	ok = do_expect_waiter(Pid3LE),
	ok = do_expect_waiter(Pid3EQ),
	ok = do_expect_waiter(undefined),
	ok = gen_tcp:close(Sock2),
	ok = do_expect_waiter(Pid4LT),
	ok = do_expect_waiter(Pid4LE),
	ok = do_expect_waiter(Pid4EQ),
	ok = do_expect_waiter(undefined),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

do_create_waiter(ReplyTo, Ref, Op, NumConns) ->
	spawn(fun () -> ok = ranch:wait_for_connections(Ref, Op, NumConns, 100),
		ReplyTo ! {wait_connections, self()} end).

do_expect_waiter(WaiterPid) ->
	receive
		{wait_connections, _} when WaiterPid=:=undefined ->
			error;
		{wait_connections, Pid} when Pid=:=WaiterPid ->
			ok
	after 1000 ->
			case WaiterPid of
				undefined ->
					ok;
				_ ->
					timeout
			end
	end.

misc_multiple_ip_local_socket_opts(_) ->
	case do_os_supports_local_sockets() of
		true ->
			do_misc_multiple_ip_local_socket_opts();
		false ->
			{skip, "No local socket support."}
	end.

do_misc_multiple_ip_local_socket_opts() ->
	doc("Ensure that a listener uses the expected ip option if multiple are given."),
	Name = name(),
	SockFile1 = do_tempname(),
	SockFile2 = do_tempname(),
	Opts = [{ip, {local, SockFile1}}, {ip, {local, SockFile2}}],
	{ok, _} = ranch:start_listener(Name, ranch_tcp, #{socket_opts => Opts}, echo_protocol, []),
	{local, SockFile2} = ranch:get_addr(Name),
	%% Make sure the socket file from the ignored ip option
	%% has not been created.
	{error, enoent} = file:read_file_info(SockFile1),
	ok = ranch:stop_listener(Name),
	%% Make sure the socket file from the accepted ip option
	%% is removed.
	{error, enoent} = file:read_file_info(SockFile2),
	ok.

misc_connection_alarms(_) ->
	doc("Ensure that connection alarms work."),
	Name = name(),

	Self = self(),
	TransOpts0 = #{num_conns_sups => 1},
	AlarmCallback = fun (Ref, AlarmName, _, ActiveConns) ->
		Self ! {connection_alarm, {Ref, AlarmName, length(ActiveConns)}}
	end,
	Alarms0 = #{
		test1 => Alarm1 = #{type => num_connections, treshold => 2, cooldown => 0, callback => AlarmCallback},
		test2 => Alarm2 = #{type => num_connections, treshold => 3, cooldown => 0, callback => AlarmCallback}
	},
	ConnectOpts = [binary, {active, false}, {packet, raw}],

	{ok, _} = ranch:start_listener(Name, ranch_tcp,
		TransOpts0#{alarms => Alarms0}, notify_and_wait_protocol, #{pid => self()}),
	Port = ranch:get_port(Name),

	{ok, _} = gen_tcp:connect("localhost", Port, ConnectOpts),
	{1, [Conn1]} = receive_loop(connected, 100),
	#{test1 := undefined, test2 := undefined} = do_recv_connection_alarms(Name, 100),

	{ok, _} = gen_tcp:connect("localhost", Port, ConnectOpts),
	{1, [Conn2]} = receive_loop(connected, 100),
	#{test1 := 2, test2 := undefined} = do_recv_connection_alarms(Name, 100),

	{ok, _} = gen_tcp:connect("localhost", Port, ConnectOpts),
	{1, [Conn3]} = receive_loop(connected, 100),
	#{test1 := 3, test2 := 3} = do_recv_connection_alarms(Name, 100),

	Alarms1 = #{
		test1 => Alarm1#{cooldown => 100},
		test2 => Alarm2#{cooldown => 100}
	},
	ok = ranch:set_transport_options(Name, TransOpts0#{alarms => Alarms1}),
	ok = do_flush_connection_alarms(Name),
	#{test1 := 3, test2 := 3} = do_recv_connection_alarms(Name, 100),
	ok = do_flush_connection_alarms(Name),
	#{test1 := 3, test2 := 3} = do_recv_connection_alarms(Name, 100),

	Conn3 ! stop,
	timer:sleep(100),
	ok = do_flush_connection_alarms(Name),
	#{test1 := 2, test2 := undefined} = do_recv_connection_alarms(Name, 100),

	Conn2 ! stop,
	timer:sleep(100),
	ok = do_flush_connection_alarms(Name),
	#{test1 := undefined, test2 := undefined} = do_recv_connection_alarms(Name, 100),

	Conn1 ! stop,

	ok = ranch:stop_listener(Name),
	ok.

do_recv_connection_alarms(Name, Timeout) ->
	do_recv_connection_alarms(Name, Timeout, #{test1 => undefined, test2 => undefined}).

do_recv_connection_alarms(Name, Timeout, Acc) ->
	receive {connection_alarm, {Name, AlarmName, N}} ->
		do_recv_connection_alarms(Name, Timeout, Acc#{AlarmName => N})
	after Timeout ->
		Acc
	end.

do_flush_connection_alarms(Name) ->
	receive {connection_alarm, {Name, _, _}} ->
		do_flush_connection_alarms(Name)
	after 0 ->
		ok
	end.

misc_stop_unknown_listener(_) ->
	doc("Ensure that stopping an unknown listener returns an error."),
	{error, not_found} = ranch:stop_listener(make_ref()),
	ok.

%% ssl.

ssl_accept_error(_) ->
	doc("Acceptor must not crash if client disconnects in the middle of SSL handshake."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, ListenerSup} = ranch:start_listener(Name,
		ranch_ssl, #{num_acceptors => 1, socket_opts => Opts},
		echo_protocol, []),
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

ssl_10_acceptors_10_listen_sockets(_) ->
	case do_os_supports_reuseport() of
		true ->
			ok = do_ssl_10_acceptors_10_listen_sockets();
		false ->
			{skip, "No SO_REUSEPORT support."}
	end.

do_ssl_10_acceptors_10_listen_sockets() ->
	doc("Ensure that we can use 10 listen sockets across 10 acceptors with SSL."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, ListenerSupPid} = ranch:start_listener(Name,
		ranch_ssl, #{
			num_acceptors => 10,
			num_listen_sockets => 10,
			socket_opts => [{raw, 1, 15, <<1:32/native>>}|Opts]},
		echo_protocol, []),
	10 = length(do_get_listener_sockets(ListenerSupPid)),
	ok = ranch:stop_listener(Name),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_many_listen_sockets_no_reuseport(_) ->
	case do_os_supports_reuseport() of
		true ->
			ok = do_ssl_many_listen_sockets_no_reuseport();
		false ->
			{skip, "No SO_REUSEPORT support."}
	end.

do_ssl_many_listen_sockets_no_reuseport() ->
	doc("Confirm that ranch:start_listener/5 fails when SO_REUSEPORT is not available with SSL."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{error, eaddrinuse} = ranch:start_listener(Name,
		ranch_ssl, #{
			num_acceptors => 10,
			num_listen_sockets => 10,
			socket_opts => [{raw, 1, 15, <<0:32/native>>}|Opts]},
		echo_protocol, []),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_active_echo(_) ->
	doc("Ensure that active mode works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts ++ [{verify, verify_none}],
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_active_n_echo(_) ->
	case do_get_ssl_version() >= {9, 2, 0} of
		true ->
			do_ssl_active_n_echo();
		false ->
			{skip, "No Active N support."}
	end.

do_ssl_active_n_echo() ->
	doc("Ensure that active N mode works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts ++ [{verify, verify_none}],
		batch_echo_protocol, [{batch_size, 3}]),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok = ssl:send(Socket, <<"One">>),
	{ok, <<"OK">>} = ssl:recv(Socket, 2, 1000),
	ok = ssl:send(Socket, <<"Two">>),
	{ok, <<"OK">>} = ssl:recv(Socket, 2, 1000),
	ok = ssl:send(Socket, <<"Three">>),
	{ok, <<"OK">>} = ssl:recv(Socket, 2, 1000),
	{ok, <<"OneTwoThree">>} = ssl:recv(Socket, 11, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_echo(_) ->
	doc("Ensure that passive mode works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts ++ [{verify, verify_none}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_handshake(_) ->
	doc("Ensure that multiple steps handshake works with SSL transport."),
	Name = name(),
	{CaCert1, Cert1, Key1} = ct_helper:make_certs(),
	{CaCert2, Cert2, Key2} = ct_helper:make_certs(),
	Opts1 = [{cert, Cert1}, {key, Key1}, {cacerts, [CaCert1]},
		{verify, verify_none}, {fail_if_no_peer_cert, false}],
	Opts2 = [{cert, Cert2}, {key, Key2}, {cacerts, [CaCert2]},
		{verify, verify_none}, {fail_if_no_peer_cert, false}],
	DefaultOpts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, [{handshake, hello}|DefaultOpts],
		handshake_protocol, #{"ranch1" => Opts1, "ranch2" => Opts2}),
	Port = ranch:get_port(Name),
	{ok, Socket1} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']},
		{server_name_indication, "ranch1"}], 5000),
	{ok, Cert1} = ssl:peercert(Socket1),
	ok = ssl:send(Socket1, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket1, 21, 1000),
	{ok, Socket2} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']},
		{server_name_indication, "ranch2"}], 5000),
	{ok, Cert2} = ssl:peercert(Socket2),
	ok = ssl:send(Socket2, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket2, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket1, 0, 1000),
	{error, closed} = ssl:recv(Socket2, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_handshake_error(_) ->
	doc("Acceptor must not crash if client disconnects in the middle of SSL handshake."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, #{num_acceptors => 1, socket_opts => Opts},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	receive after 500 -> ok end,
	[ConnPid] = ranch:procs(Name, connections),
	ConnMon = monitor(process, ConnPid),
	ok = gen_tcp:send(Socket, <<"GARBAGE">>),
	ok = gen_tcp:close(Socket),
	receive
		{'DOWN', ConnMon, process, ConnPid, R} ->
			{shutdown, {_, _}} = R
	after 1000 ->
		error(timeout)
	end,
	receive after 500 -> ok end,
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_local_echo(_) ->
	case do_os_supports_local_sockets() of
		true ->
			do_ssl_local_echo();
		false ->
			{skip, "No local socket support."}
	end.

do_ssl_local_echo() ->
	doc("Ensure that listening on a local socket works with SSL transport."),
	SockFile = do_tempname(),
	try
		Name = name(),
		Opts = ct_helper:get_certs_from_ets(),
		{ok, _} = ranch:start_listener(Name,
			ranch_ssl, #{socket_opts => [{ip, {local, SockFile}}|Opts] ++ [{verify, verify_none}]},
			echo_protocol, []),
		undefined = ranch:get_port(Name),
		{ok, Socket} = ssl:connect({local, SockFile}, 0, [
			binary, {active, false}, {packet, raw},
			{verify, verify_none}, {versions, ['tlsv1.2']}]),
		ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
		{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
		ok = ranch:stop_listener(Name),
		{error, closed} = ssl:recv(Socket, 0, 1000),
		%% Make sure the listener stopped.
		{'EXIT', _} = begin catch ranch:get_port(Name) end,
		%% Make sure the socket file is removed.
		{error, enoent} = file:read_file_info(SockFile),
		ok
	after
		file:delete(SockFile)
	end.

ssl_sni_echo(_) ->
	doc("Ensure that SNI works with SSL transport."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, [{sni_hosts, [{"localhost", Opts ++ [{verify, verify_none}]}]}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_sni_fail(_) ->
	doc("Ensure that connection fails when host is not in SNI list."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, [{sni_hosts, [{"pouet", Opts}]}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	%% We stick to TLS 1.2 because there seems to be a bug in OTP-23.0rc2
	%% that leads to a malformed_handshake_data error.
	{error, _} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_tls_psk(_) ->
	doc("Ensure that TLS-PSK works without certificate."),
	Name = name(),
	Ciphers = [#{cipher => aes_256_gcm, key_exchange => psk, mac => aead, prf => sha384}],
	LookupFun = {fun psk_lookup_helper/3, <<"shared_secret">>},
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, [{ciphers, Ciphers}, {user_lookup_fun, LookupFun}, {versions, ['tlsv1.2']}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [
		binary, {active, false}, {ciphers, Ciphers},
		{user_lookup_fun, LookupFun}, {verify, verify_none}, {versions, ['tlsv1.2']}
	]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_tls_psk_fail(_) ->
	doc("Ensure that TLS-PSK filed for different shared keys."),
	Name = name(),
	Ciphers = [#{cipher => aes_256_gcm, key_exchange => psk, mac => aead, prf => sha384}],
	ServerLookupFun = {fun psk_lookup_helper/3, <<"server_secret">>},
	ClientLookupFun = {fun psk_lookup_helper/3, <<"client_secret">>},
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, [{ciphers, Ciphers}, {user_lookup_fun, ServerLookupFun}, {versions, ['tlsv1.2']}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, _} = ssl:connect("localhost", Port, [
		binary, {active, false}, {ciphers, Ciphers},
		{user_lookup_fun, ClientLookupFun}, {verify, verify_none}, {versions, ['tlsv1.2']}
	]),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

psk_lookup_helper(psk, _PskIdentity, UserState) ->
	{ok, UserState}.

ssl_upgrade_from_tcp(_) ->
	doc("Ensure a TCP socket can be upgraded to SSL"),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		ssl_upgrade_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"ECHO Before upgrading to SSL">>),
	{ok, <<"Before upgrading to SSL">>} = gen_tcp:recv(Socket, 23, 1000),
	ok = gen_tcp:send(Socket, <<"UPGRADE">>),
	{ok, <<"READY">>} = gen_tcp:recv(Socket, 5, 1000),
	{ok, SslSocket} = ssl:connect(Socket,
		[{verify, verify_none}, {versions, ['tlsv1.2']}],
		5000),
	ok = ssl:send(SslSocket, <<"ECHO After upgrading to SSL">>),
	{ok, <<"After upgrading to SSL">>} = ssl:recv(SslSocket, 22, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(SslSocket, 0, 1000),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_graceful(_) ->
	doc("Ensure suspending and resuming of listeners does not kill active connections."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts ++ [{verify, verify_none}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	%% Make sure connections with a fresh listener work.
	running = ranch:get_status(Name),
	{ok, Socket1} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok = ssl:send(Socket1, <<"SSL with fresh listener">>),
	{ok, <<"SSL with fresh listener">>} = ssl:recv(Socket1, 23, 1000),
	%% Suspend listener, make sure established connections keep running.
	ok = ranch:suspend_listener(Name),
	suspended = ranch:get_status(Name),
	ok = ssl:send(Socket1, <<"SSL with suspended listener">>),
	{ok, <<"SSL with suspended listener">>} = ssl:recv(Socket1, 27, 1000),
	%% Make sure new connections are refused on the suspended listener.
	{error, econnrefused} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	%% Make sure transport options can be changed when listener is suspended.
	ok = ranch:set_transport_options(Name, #{socket_opts => [{port, Port}|Opts] ++ [{verify, verify_none}]}),
	%% Resume listener, make sure connections can be established again.
	ok = ranch:resume_listener(Name),
	running = ranch:get_status(Name),
	{ok, Socket2} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok = ssl:send(Socket2, <<"SSL with resumed listener">>),
	{ok, <<"SSL with resumed listener">>} = ssl:recv(Socket2, 25, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = ssl:recv(Socket1, 0, 1000),
	{error, closed} = ssl:recv(Socket2, 0, 1000),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_getopts_capability(_) ->
	doc("Ensure getopts/2 capability."),
	Name=name(),
	Opts=ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts ++ [{verify, verify_none}],
		transport_capabilities_protocol, []),
	Port=ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok=ssl:send(Socket, <<"getopts/2">>),
	{ok, <<"OK">>}=ssl:recv(Socket, 0, 1000),
	ok=ranch:stop_listener(Name),
	{error, closed}=ssl:recv(Socket, 0, 1000),
	{'EXIT', _}=begin catch ranch:get_port(Name) end,
	ok.

ssl_getstat_capability(_) ->
	doc("Ensure getstat/1,2 capability."),
	Name=name(),
	Opts=ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts ++ [{verify, verify_none}],
		transport_capabilities_protocol, []),
	Port=ranch:get_port(Name),
	{ok, Socket} = ssl:connect("localhost", Port, [
		binary, {active, false}, {packet, raw},
		{verify, verify_none}, {versions, ['tlsv1.2']}]),
	ok=ssl:send(Socket, <<"getstat/1">>),
	{ok, <<"OK">>}=ssl:recv(Socket, 0, 1000),
	ok=ssl:send(Socket, <<"getstat/2">>),
	{ok, <<"OK">>}=ssl:recv(Socket, 0, 1000),
	ok=ranch:stop_listener(Name),
	{error, closed}=ssl:recv(Socket, 0, 1000),
	{'EXIT', _}=begin catch ranch:get_port(Name) end,
	ok.

ssl_error_eaddrinuse(_) ->
	doc("Ensure that failure due to an eaddrinuse returns a compact readable error."),
	Name = name(),
	Opts = ct_helper:get_certs_from_ets(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, Opts,
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, eaddrinuse} = ranch:start_listener({Name, fails},
		ranch_ssl, [{port, Port}|Opts],
		active_echo_protocol, []),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

ssl_error_no_cert(_) ->
	doc("Ensure that failure due to missing certificate returns a compact readable error."),
	{error, no_cert} = ranch:start_listener(name(),
		ranch_ssl, #{},
		active_echo_protocol, []),
	ok.

ssl_error_eacces(_) ->
	case os:type() of
		{win32, nt} ->
			{skip, "No privileged ports."};
		{unix, darwin} ->
			{skip, "No privileged ports."};
		_ ->
			doc("Ensure that failure due to an eacces returns a compact readable error."),
			Name = name(),
			Opts = ct_helper:get_certs_from_ets(),
			{error, eacces} = ranch:start_listener(Name,
				ranch_ssl, [{port, 283}|Opts],
				active_echo_protocol, []),
			ok
	end.

ssl_unsupported_tlsv13_options(_) ->
	{available, Versions} = lists:keyfind(available, 1, ssl:versions()),
	case {lists:member('tlsv1.3', Versions), do_get_ssl_version() >= {10, 0, 0}} of
		{true, true} ->
			do_ssl_unsupported_tlsv13_options();
		{false, _} ->
			{skip, "No TLSv1.3 support."};
		{_, false} ->
			{skip, "No TLSv1.3 option dependency checking."}
	end.

do_ssl_unsupported_tlsv13_options() ->
	doc("Ensure that a listener can be started when TLSv1.3 is "
	    "the only protocol and unsupported options are present."),
	CheckOpts = [
		{beast_mitigation, one_n_minus_one},
		{client_renegotiation, true},
		{next_protocols_advertised, [<<"dummy">>]},
		{padding_check, true},
		{psk_identity, "dummy"},
		{secure_renegotiate, true},
		{reuse_session, fun (_, _, _, _) -> true end},
		{reuse_sessions, true},
		{user_lookup_fun, {fun (_, _, _) -> error end, <<"dummy">>}}
	],
	Name = name(),
	Opts = ct_helper:get_certs_from_ets() ++ [{versions, ['tlsv1.3']}],
	ok = lists:foreach(
		fun (CheckOpt) ->
			Opts1 = Opts ++ [CheckOpt],
			case ssl:listen(0, Opts1) of
				{error, {options, dependency, _}} -> ok; %% Before OTP-26.
				{error, {options, incompatible, _}} -> ok %% OTP-26+.
			end,
			{ok, _} = ranch:start_listener(Name,
				ranch_ssl, #{socket_opts => Opts1},
				echo_protocol, []),
			ok = ranch:stop_listener(Name)
		end,
		CheckOpts
	),
	ok.

%% tcp.

tcp_10_acceptors_10_listen_sockets(Config) ->
	case do_os_supports_reuseport() of
		true ->
			ok = do_tcp_10_acceptors_10_listen_sockets(Config);
		false ->
			{skip, "No SO_REUSEPORT support."}
	end.

do_tcp_10_acceptors_10_listen_sockets(Config) ->
	doc("Ensure that we can use 10 listen sockets across 10 acceptors with TCP."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	Self = self(),
	Tag = make_ref(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{
			num_acceptors => 10,
			num_listen_sockets => 10,
			socket_opts => SockOpts ++ [{raw, 1, 15, <<1:32/native>>}],
			post_listen_callback => fun (LSocket) -> Self ! {Tag, LSocket}, ok end},
		echo_protocol, []),
	LSockets = [receive {Tag, LSocket} -> LSocket after 1000 -> error(timeout) end
		|| _ <- lists:seq(1, 10)],
	10 = length(LSockets),
	10 = length(lists:usort(LSockets)),
	ok = ranch:stop_listener(Name),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_many_listen_sockets_no_reuseport(Config) ->
	case do_os_supports_reuseport() of
		true ->
			ok = do_tcp_many_listen_sockets_no_reuseport(Config);
		false ->
			{skip, "No SO_REUSEPORT support."}
	end.

do_tcp_many_listen_sockets_no_reuseport(Config) ->
	doc("Confirm that ranch:start_listener/5 fails when SO_REUSEPORT is not available with TCP."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{error, eaddrinuse} = ranch:start_listener(Name,
		ranch_tcp, #{
			num_acceptors => 10,
			num_listen_sockets => 10,
			socket_opts => SockOpts ++ [{raw, 1, 15, <<0:32/native>>}]},
		echo_protocol, []),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_active_echo(Config) ->
	doc("Ensure that active mode works with TCP transport."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_active_n_echo(Config) ->
	doc("Ensure that active N mode works with TCP transport."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		batch_echo_protocol, [{batch_size, 3}]),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"One">>),
	{ok, <<"OK">>} = gen_tcp:recv(Socket, 2, 1000),
	ok = gen_tcp:send(Socket, <<"Two">>),
	{ok, <<"OK">>} = gen_tcp:recv(Socket, 2, 1000),
	ok = gen_tcp:send(Socket, <<"Three">>),
	{ok, <<"OK">>} = gen_tcp:recv(Socket, 2, 1000),
	{ok, <<"OneTwoThree">>} = gen_tcp:recv(Socket, 11, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_echo(Config) ->
	doc("Ensure that passive mode works with TCP transport."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_local_echo(_) ->
	case do_os_supports_local_sockets() of
		true ->
			do_tcp_local_echo();
		false ->
			{skip, "No local socket support."}
	end.

do_tcp_local_echo() ->
	doc("Ensure that listening on a local socket works with TCP transport."),
	SockFile = do_tempname(),
	try
		Name = name(),
		{ok, _} = ranch:start_listener(Name,
			ranch_tcp, #{socket_opts => [{ip, {local, SockFile}}]},
			echo_protocol, []),
		undefined = ranch:get_port(Name),
		{ok, Socket} = gen_tcp:connect({local, SockFile}, 0, [binary, {active, false}, {packet, raw}]),
		ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
		{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
		ok = ranch:stop_listener(Name),
		{error, closed} = gen_tcp:recv(Socket, 0, 1000),
		%% Make sure the listener stopped.
		{'EXIT', _} = begin catch ranch:get_port(Name) end,
		%% Make sure the socket file is removed.
		{error, enoent} = file:read_file_info(SockFile),
		ok
	after
		file:delete(SockFile)
	end.

tcp_graceful(Config) ->
	doc("Ensure suspending and resuming of listeners does not kill active connections."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	%% Make sure connections with a fresh listener work.
	running = ranch:get_status(Name),
	{ok, Socket1} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket1, <<"TCP with fresh listener">>),
	{ok, <<"TCP with fresh listener">>} = gen_tcp:recv(Socket1, 23, 1000),
	%% Suspend listener, make sure established connections keep running.
	ok = ranch:suspend_listener(Name),
	suspended = ranch:get_status(Name),
	ok = gen_tcp:send(Socket1, <<"TCP with suspended listener">>),
	{ok, <<"TCP with suspended listener">>} = gen_tcp:recv(Socket1, 27, 1000),
	%% Make sure new connections are refused on the suspended listener.
	{error, econnrefused} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	%% Make sure transport options can be changed when listener is suspended.
	ok = ranch:set_transport_options(Name, [{port, Port}]),
	%% Resume listener, make sure connections can be established again.
	ok = ranch:resume_listener(Name),
	running = ranch:get_status(Name),
	{ok, Socket2} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket2, <<"TCP with resumed listener">>),
	{ok, <<"TCP with resumed listener">>} = gen_tcp:recv(Socket2, 25, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket1, 0, 1000),
	{error, closed} = gen_tcp:recv(Socket2, 0, 1000),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_inherit_options(Config) ->
	doc("Ensure TCP options are inherited in the protocol."),
	Name = name(),
	Opts0 = config(socket_opts, Config),
	Opts1 = [{nodelay, false}, {send_timeout_close, false}],
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => Opts0 ++ Opts1},
		check_tcp_options, [{pid, self()} | Opts1]),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, raw}]),
	receive checked -> ok after 1000 -> error(timeout) end,
	ok = gen_tcp:close(Socket),
	ok = ranch:stop_listener(Name).

tcp_max_connections(Config) ->
	doc("Ensure the max_connections option actually limits connections."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 10, num_acceptors => 1, socket_opts => SockOpts},
		notify_and_wait_protocol, #{pid => self()}),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 11, 150),
	10 = ranch_server:count_connections(Name),
	{10, Pids1} = receive_loop(connected, 400),
	ok = terminate_loop(stop, Pids1),
	{1, Pids2} = receive_loop(connected, 1000),
	ok = terminate_loop(stop, Pids2),
	ok = ranch:stop_listener(Name).

tcp_max_connections_and_beyond(Config) ->
	doc("Ensure the max_connections option works when connections are removed from the count."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 10, num_acceptors => 1, socket_opts => SockOpts},
		remove_conn_and_wait_protocol, [{remove, true, 2500}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	0 = ranch_server:count_connections(Name),
	10 = length(do_conns_which_children(Name)),
	Counts = do_conns_count_children(Name),
	{_, 1} = lists:keyfind(specs, 1, Counts),
	{_, 0} = lists:keyfind(supervisors, 1, Counts),
	{_, 10} = lists:keyfind(active, 1, Counts),
	{_, 10} = lists:keyfind(workers, 1, Counts),
	ranch:set_protocol_options(Name, [{remove, false, 2500}]),
	receive after 250 -> ok end,
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	10 = ranch_server:count_connections(Name),
	20 = length(do_conns_which_children(Name)),
	Counts2 = do_conns_count_children(Name),
	{_, 20} = lists:keyfind(active, 1, Counts2),
	{_, 20} = lists:keyfind(workers, 1, Counts2),
	ok = ranch:stop_listener(Name).

tcp_max_connections_infinity(Config) ->
	doc("Set the max_connections option from 10 to infinity and back to 10."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 10, num_acceptors => 1, socket_opts => SockOpts},
		notify_and_wait_protocol, #{pid => self()}),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 20, 0),
	10 = ranch_server:count_connections(Name),
	{10, Pids1} = receive_loop(connected, 1000),
	10 = ranch_server:count_connections(Name),
	10 = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, infinity),
	receive after 250 -> ok end,
	20 = ranch_server:count_connections(Name),
	infinity = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, 10),
	20 = ranch_server:count_connections(Name),
	{10, Pids2} = receive_loop(connected, 1000),
	ok = terminate_loop(stop, Pids1 ++ Pids2),
	ok = ranch:stop_listener(Name).

tcp_remove_connections(Config) ->
	doc("Ensure that removed connections are only removed once."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		remove_conn_and_wait_protocol, [{remove, true, 0}]),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 10, 0),
	receive after 250 -> ok end,
	0 = ranch_server:count_connections(Name),
	ok = ranch:stop_listener(Name).

tcp_remove_connections_acceptor_wakeup(Config) ->
	doc("Ensure that removed connections wake up acceptors."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 1, num_acceptors => 1, socket_opts => SockOpts},
		remove_conn_and_wait_protocol, [{remove, true, infinity}]),
	Port = ranch:get_port(Name),
	ConnectOptions = [binary, {active, false}],
	Localhost = "localhost",
	{ok, Socket1} = gen_tcp:connect(Localhost, Port, ConnectOptions),
	{ok, Socket2} = gen_tcp:connect(Localhost, Port, ConnectOptions),
	{ok, Socket3} = gen_tcp:connect(Localhost, Port, ConnectOptions),
	ok = gen_tcp:send(Socket3, <<"bye">>),
	true = maps:get(all_connections, ranch:info(Name)) >= 2,
	ok = gen_tcp:send(Socket1, <<"bye">>),
	ok = gen_tcp:send(Socket2, <<"bye">>),
	ok = ranch:stop_listener(Name).

tcp_set_max_connections(Config) ->
	doc("Ensure that changing the max_connections option to a larger value allows for more connections."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 10, num_acceptors => 1, socket_opts => SockOpts},
		notify_and_wait_protocol, #{pid => self()}),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 20, 0),
	10 = ranch_server:count_connections(Name),
	{10, Pids1} = receive_loop(connected, 1000),
	10 = ranch:get_max_connections(Name),
	ranch:set_max_connections(Name, 20),
	{10, Pids2} = receive_loop(connected, 1000),
	20 = ranch:get_max_connections(Name),
	ok = terminate_loop(stop, Pids1 ++ Pids2),
	ok = ranch:stop_listener(Name).

tcp_set_max_connections_clean(Config) ->
	doc("Ensure that setting max_connections does not crash any process."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, ListSupPid} = ranch:start_listener(Name,
		ranch_tcp, #{max_connections => 4, socket_opts => SockOpts},
		notify_and_wait_protocol, #{pid => self()}),
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

tcp_getopts_capability(Config) ->
	doc("Ensure getopts/2 capability."),
	Name=name(),
	SockOpts = config(socket_opts, Config),
	{ok, _}=ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		transport_capabilities_protocol, []),
	Port=ranch:get_port(Name),
	{ok, Socket}=gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok=gen_tcp:send(Socket, <<"getopts/2">>),
	{ok, <<"OK">>}=gen_tcp:recv(Socket, 0, 1000),
	ok=ranch:stop_listener(Name),
	{error, closed}=gen_tcp:recv(Socket, 0, 1000),
	{'EXIT', _}=begin catch ranch:get_port(Name) end,
	ok.

tcp_getstat_capability(Config) ->
	doc("Ensure getstat/1,2 capability."),
	Name=name(),
	SockOpts = config(socket_opts, Config),
	{ok, _}=ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		transport_capabilities_protocol, []),
	Port=ranch:get_port(Name),
	{ok, Socket}=gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok=gen_tcp:send(Socket, <<"getstat/1">>),
	{ok, <<"OK">>}=gen_tcp:recv(Socket, 0, 1000),
	ok=gen_tcp:send(Socket, <<"getstat/2">>),
	{ok, <<"OK">>}=gen_tcp:recv(Socket, 0, 1000),
	ok=ranch:stop_listener(Name),
	{error, closed}=gen_tcp:recv(Socket, 0, 1000),
	{'EXIT', _}=begin catch ranch:get_port(Name) end,
	ok.

tcp_upgrade(Config) ->
	doc("Ensure that protocol options can be updated."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		notify_and_wait_protocol, #{pid => self()}),
	Port = ranch:get_port(Name),
	ok = connect_loop(Port, 1, 0),
	{1, Pids1} = receive_loop(connected, 1000),
	ranch:set_protocol_options(Name, #{msg => upgraded, pid => self()}),
	ok = connect_loop(Port, 1, 0),
	{1, Pids2} = receive_loop(upgraded, 1000),
	ok = terminate_loop(stop, Pids1 ++ Pids2),
	ok = ranch:stop_listener(Name).

tcp_error_eaddrinuse(Config) ->
	doc("Ensure that failure due to an eaddrinuse returns a compact readable error."),
	Name = name(),
	SockOpts = config(socket_opts, Config),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{socket_opts => SockOpts},
		active_echo_protocol, []),
	Port = ranch:get_port(Name),
	{error, eaddrinuse} = ranch:start_listener({Name, fails},
		ranch_tcp, [{port, Port}],
		active_echo_protocol, []),
	ok = ranch:stop_listener(Name),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

tcp_error_eacces(Config) ->
	case os:type() of
		{win32, nt} ->
			{skip, "No privileged ports."};
		{unix, darwin} ->
			{skip, "No privileged ports."};
		_ ->
			doc("Ensure that failure due to an eacces returns a compact readable error."),
			Name = name(),
			SockOpts = config(socket_opts, Config),
			{error, eacces} = ranch:start_listener(Name,
				ranch_tcp, #{socket_opts => SockOpts ++ [{port, 283}]},
				active_echo_protocol, []),
			ok
	end.

%% Supervisor tests

connection_type_supervisor(_) ->
	doc("The supervisor connection type must be reflected in the specifications."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{connection_type => supervisor},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	[{echo_protocol, _, supervisor, [echo_protocol]}] = do_conns_which_children(Name),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

connection_type_supervisor_separate_from_connection(_) ->
	doc("The supervisor connection type allows separate supervised and connection processes."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{connection_type => supervisor},
		supervisor_separate, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	[{supervisor_separate, _, supervisor, [supervisor_separate]}] = do_conns_which_children(Name),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

supervisor_10_acceptors_1_conns_sup(_) ->
	doc("Ensure that using 10 acceptors and 1 connection supervisor works."),
	ok = do_supervisor_n_acceptors_m_conns_sups(10, 1).

supervisor_9_acceptors_4_conns_sups(_) ->
	doc("Ensure that using 9 acceptors and 4 connection supervisors works."),
	ok = do_supervisor_n_acceptors_m_conns_sups(9, 4).

supervisor_10_acceptors_10_conns_sups(_) ->
	doc("Ensure that using 10 acceptors and 10 connection supervisors works."),
	ok = do_supervisor_n_acceptors_m_conns_sups(10, 10).

supervisor_1_acceptor_10_conns_sups(_) ->
	doc("Ensure that using 1 acceptor and 10 connection supervisors works."),
	ok = do_supervisor_n_acceptors_m_conns_sups(1, 10).

do_supervisor_n_acceptors_m_conns_sups(NumAcceptors, NumConnsSups) ->
	Name = name(),
	{ok, Pid} = ranch:start_listener(Name,
		ranch_tcp, #{num_conns_sups => NumConnsSups, num_acceptors => NumAcceptors},
		notify_and_wait_protocol, #{pid => self()}),
	Port = ranch:get_port(Name),
	ConnsSups = [ConnsSup || {_, ConnsSup} <- ranch_server:get_connections_sups(Name)],
	NumConnsSups = length(ConnsSups),
	{ranch_acceptors_sup, AcceptorsSup, supervisor, _} =
		lists:keyfind(ranch_acceptors_sup, 1, supervisor:which_children(Pid)),
	AcceptorIds = [AcceptorId ||
		{{acceptor, _, AcceptorId}, _, worker, _} <- supervisor:which_children(AcceptorsSup)],
	NumAcceptors = length(AcceptorIds),
	AcceptorConnsSups0 = [ranch_server:get_connections_sup(Name, AcceptorId) ||
		AcceptorId <- AcceptorIds],
	AcceptorConnsSups1 = lists:usort(AcceptorConnsSups0),
	if
		NumAcceptors > NumConnsSups ->
			NumConnsSups = length(AcceptorConnsSups1),
			[] = ConnsSups -- AcceptorConnsSups1;
		NumAcceptors < NumConnsSups ->
			NumAcceptors = length(AcceptorConnsSups1),
			[] = AcceptorConnsSups1 -- ConnsSups;
		NumAcceptors =:= NumConnsSups ->
			NumConnsSups = length(AcceptorConnsSups1),
			NumAcceptors = length(AcceptorConnsSups1),
			[] = ConnsSups -- AcceptorConnsSups1,
			[] = AcceptorConnsSups1 -- ConnsSups
	end,
	ok = connect_loop(Port, 100, 0),
	{100, Pids} = receive_loop(connected, 1000),
	100 = ranch_server:count_connections(Name),
	ok = terminate_loop(stop, Pids),
	ok = ranch:stop_listener(Name),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

supervisor_changed_options_restart(_) ->
	doc("Ensure that a listener is restarted with changed transport options."),
	Name = name(),
	%% Start a listener using send_timeout as option change marker.
	{ok, ListenerSupPid1} = ranch:start_listener(Name,
		ranch_tcp, [{send_timeout, 300000}],
		echo_protocol, []),
	%% Ensure send_timeout is really set to initial value.
	{ok, [{send_timeout, 300000}]}
		= inet:getopts(do_get_listener_socket(ListenerSupPid1), [send_timeout]),
	%% Change send_timeout option.
	ok = ranch:suspend_listener(Name),
	ok = ranch:set_transport_options(Name, [{send_timeout, 300001}]),
	ok = ranch:resume_listener(Name),
	%% Ensure send_timeout is really set to the changed value.
	{ok, [{send_timeout, 300001}]}
		= inet:getopts(do_get_listener_socket(ListenerSupPid1), [send_timeout]),
	%% Crash the listener_sup process, allow a short time for restart to succeed.
	%% We silence the expected log events coming from the relevant supervisors.
	ListenerChilds = [ChildPid || {_, ChildPid, _, _} <- supervisor:which_children(ListenerSupPid1)],
	FilterFun = fun (#{meta := #{pid := EventPid}}, _) ->
		case lists:member(EventPid, ListenerChilds) of
			true -> stop;
			false -> ignore
		end
	end,
	ok = logger:add_primary_filter(?MODULE, {FilterFun, undefined}),
	try
		exit(ListenerSupPid1, kill),
		timer:sleep(1000)
	after
		ok = logger:remove_primary_filter(?MODULE)
	end,
	%% Obtain pid of restarted listener_sup process.
	[ListenerSupPid2] = [Pid || {{ranch_listener_sup, Ref}, Pid, supervisor, _}
		<- supervisor:which_children(ranch_sup), Ref =:= Name],
	%% Ensure send_timeout is still set to the changed value.
	{ok, [{send_timeout, 300001}]}
		= inet:getopts(do_get_listener_socket(ListenerSupPid2), [send_timeout]),
	ok = ranch:stop_listener(Name),
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

supervisor_clean_child_restart(_) ->
	doc("Verify that only the relevant parts of the supervision tree restarted "
		"when the listening socket is closed."),
	Name = name(),
	%% Trace socket allocations.
	{module, ranch_tcp} = code:ensure_loaded(ranch_tcp),
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, Pid} = ranch:start_listener(Name,
		ranch_tcp, #{num_acceptors => 1},
		echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	ConnsSups = ranch_server:get_connections_sups(Name),
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
	true = lists:all(fun erlang:is_process_alive/1, [ConnsSup || {_, ConnsSup} <- ConnsSups]),
	%% Check that acceptors_sup is restarted properly.
	AccSupPid = receive {trace, Pid, spawn, Pid1, _} -> Pid1 end,
	receive {trace, AccSupPid, spawn, _, _} -> ok end,
	%% No more traces then.
	receive
		{trace, _, spawn, _, _} -> error(invalid_restart)
	after 1000 -> ok end,
	%% Verify that children still registered right.
	ConnsSups = ranch_server:get_connections_sups(Name),
	_ = erlang:trace_pattern({ranch_tcp, listen, 1}, false, []),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

supervisor_clean_restart(_) ->
	doc("Verify that killing ranch_conns_sup does not crash everything "
		"and that it restarts properly."),
	Name = name(),
	NumAcc = 4,
	{ok, Pid} = ranch:start_listener(Name,
		ranch_tcp, #{num_acceptors => NumAcc},
		echo_protocol, []),
	%% Trace supervisor spawns.
	1 = erlang:trace(Pid, true, [procs, set_on_spawn]),
	{_, ConnsSupSup0, _, _} = lists:keyfind(ranch_conns_sup_sup, 1, supervisor:which_children(Pid)),
	exit(ConnsSupSup0, kill),
	receive after 1000 -> ok end,
	%% Verify that supervisor is alive
	true = is_process_alive(Pid),
	%% ...but children are dead.
	false = is_process_alive(ConnsSupSup0),
	%% Receive traces from newly started children
	ConnsSupSup = receive {trace, Pid, spawn, Pid2, _} -> Pid2 end,
	[receive {trace, ConnsSupSup, spawn, _Pid, _} -> ok end ||
		_ <- lists:seq(1, NumAcc)],
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
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces(),
	ok = ranch:stop_listener(Name).

supervisor_conns_alive(_) ->
	doc("Ensure that active connections stay open when the listening socket gets closed."),
	Name = name(),
	{module, ranch_tcp} = code:ensure_loaded(ranch_tcp),
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_tcp, listen, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
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

supervisor_embedded_ranch_server_crash(_) ->
	doc("Ensure that restarting ranch_server also restarts embedded listeners."),
	Name = name(),
	{ok, SupPid} = embedded_sup:start_link(),
	{ok, EmbeddedSupPid} = embedded_sup:start_listener(SupPid, Name,
		ranch_tcp, #{},
		echo_protocol, []),
	[{{ranch_listener_sup, Name}, ListenerPid, supervisor, _},
		{ranch_server_proxy, ProxyPid, worker, _}] = supervisor:which_children(EmbeddedSupPid),
	ProxyMonitor = monitor(process, ProxyPid),
	ListenerMonitor = monitor(process, ListenerPid),
	ok = supervisor:terminate_child(ranch_sup, ranch_server),
	receive {'DOWN', ProxyMonitor, process, ProxyPid, shutdown} -> ok after 1000 -> exit(timeout) end,
	receive {'DOWN', ListenerMonitor, process, ListenerPid, shutdown} -> ok after 1000 -> exit(timeout) end,
	{ok, _} = supervisor:restart_child(ranch_sup, ranch_server),
	receive after 1000 -> ok end,
	[{{ranch_listener_sup, Name}, _, supervisor, _},
		{ranch_server_proxy, _, worker, _}] = supervisor:which_children(EmbeddedSupPid),
	embedded_sup:stop_listener(SupPid, Name),
	embedded_sup:stop(SupPid),
	ok.

supervisor_protocol_start_link_crash(_) ->
	doc("Ensure a protocol start crash does not kill all connections."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		crash_protocol, []),
	ConnsSups = ranch_server:get_connections_sups(Name),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, [binary, {active, true}, {packet, raw}]),
	receive after 500 -> ok end,
	ConnsSups = ranch_server:get_connections_sups(Name),
	ok = ranch:stop_listener(Name).

supervisor_server_recover_state(_) ->
	doc("Ensure that when ranch_server crashes and restarts, it recovers "
		"its state and continues monitoring the same processes."),
	Name = name(),
	_ = erlang:trace(new, true, [call]),
	1 = erlang:trace_pattern({ranch_server, init, 1},
		[{'_', [], [{return_trace}]}], [global]),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		echo_protocol, []),
	ConnsSups = ranch_server:get_connections_sups(Name),
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
	ConnsSups = ranch_server:get_connections_sups(Name),
	ok = ranch:stop_listener(Name),
	%% Check ranch_server has removed the ranch_conns_sup.
	[] = (catch ranch_server:get_connections_sups(Name)),
	_ = erlang:trace(all, false, [all]),
	ok = clean_traces().

supervisor_unexpected_message(_) ->
	doc("Ensure the connections supervisor stays alive when it receives "
		"an unexpected message."),
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	%% Send the unexpected message to all ranch_conns_sups.
	_ = [ConnSup ! hello || {_, ConnSup} <- ranch_server:get_connections_sups(Name)],
	%% Connection is still up.
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(Name),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	%% Make sure the listener stopped.
	{'EXIT', _} = begin catch ranch:get_port(Name) end,
	ok.

%% Utility functions.

connect_loop(_, 0, _) ->
	ok;
connect_loop(Port, N, Sleep) ->
	{ok, _} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	receive after Sleep -> ok end,
	connect_loop(Port, N - 1, Sleep).

receive_loop(Message, Timeout) ->
	receive_loop(Message, Timeout, 0, []).
receive_loop(Message, Timeout, N, Acc) ->
	receive {Pid, Message} ->
		receive_loop(Message, Timeout, N + 1, [Pid|Acc])
	after Timeout ->
		{N, Acc}
	end.

terminate_loop(_, []) ->
	ok;
terminate_loop(Message, [Pid|Pids]) ->
	Pid ! Message,
	terminate_loop(Message, Pids).

clean_traces() ->
	receive
		{trace, _, _, _} ->
			clean_traces();
		{trace, _, _, _, _} ->
			clean_traces()
	after 0 ->
		ok
	end.

do_get_listener_socket(ListenerSupPid) ->
	[LSocket] = do_get_listener_sockets(ListenerSupPid),
	LSocket.

do_get_listener_sockets(ListenerSupPid) ->
	[AcceptorsSupPid] = [Pid || {ranch_acceptors_sup, Pid, supervisor, _}
		<- supervisor:which_children(ListenerSupPid)],
	{links, Links} = erlang:process_info(AcceptorsSupPid, links),
	[P || P <- Links, is_port(P)].

do_conns_which_children(Name) ->
	Conns = [supervisor:which_children(ConnsSup) ||
		{_, ConnsSup} <- ranch_server:get_connections_sups(Name)],
	lists:flatten(Conns).

do_conns_count_children(Name) ->
	lists:foldl(
		fun
			(Stats, undefined) ->
				Stats;
			(Stats, Acc) ->
				lists:zipwith(
					fun ({K, V1}, {K, V2}) -> {K, V1+V2} end,
					Acc,
					Stats
				)
		end,
		undefined,
		[supervisor:count_children(ConnsSup) ||
			{_, ConnsSup} <- ranch_server:get_connections_sups(Name)]
	).

do_os_supports_reuseport() ->
	case {os:type(), os:version()} of
		{{unix, linux}, {Major, _, _}} when Major > 3 -> true;
		{{unix, linux}, {3, Minor, _}} when Minor >= 9 -> true;
		_ -> false
	end.

do_os_supports_local_sockets() ->
	case os:type() of
		{unix, _} -> true;
		_ -> false
	end.

do_tempname() ->
	list_to_binary(lists:droplast(os:cmd("mktemp -u"))).

do_get_ssl_version() ->
	{ok, Vsn} = application:get_key(ssl, vsn),
	Vsns0 = re:split(Vsn, "\\D+", [{return, list}]),
	Vsns1 = lists:map(fun list_to_integer/1, Vsns0),
	case Vsns1 of
		[] -> {0, 0, 0};
		[Major] -> {Major, 0, 0};
		[Major, Minor] -> {Major, Minor, 0};
		[Major, Minor, Patch|_] -> {Major, Minor, Patch}
	end.
