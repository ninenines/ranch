%% Copyright (c) Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) Jan Uhlig <juhlig@hnc-agency.org>
%% Copyright (c) Maria Scott <maria-12648430@hnc-agency.org>
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

-module(ranch).

-export([start_listener/5]).
-export([normalize_opts/1]).
-export([stop_listener/1]).
-export([suspend_listener/1]).
-export([resume_listener/1]).
-export([stop_all_acceptors/0]).
-export([restart_all_acceptors/0]).
-export([child_spec/5]).
-export([handshake/1]).
-export([handshake/2]).
-export([handshake_continue/1]).
-export([handshake_continue/2]).
-export([handshake_cancel/1]).
-export([recv_proxy_header/2]).
-export([remove_connection/1]).
-export([remove_connection/2]).
-export([get_status/1]).
-export([get_addr/1]).
-export([get_port/1]).
-export([get_max_connections/1]).
-export([set_max_connections/2]).
-export([get_transport_options/1]).
-export([set_transport_options/2]).
-export([get_protocol_options/1]).
-export([set_protocol_options/2]).
-export([info/0]).
-export([info/1]).
-export([procs/2]).
-export([trace_acceptors/1]).
-export([trace_acceptors/2]).
-export([wait_for_connections/3]).
-export([wait_for_connections/4]).
-export([filter_options/4]).
-export([set_option_default/3]).
-export([require/1]).
-export([log/4]).

%% Internal
-export([compat_normalize_alarms_option/1]).

-type max_conns() :: non_neg_integer() | infinity.
-export_type([max_conns/0]).

-type opts() :: any() | transport_opts(any()).
-export_type([opts/0]).

-type alarm(Type, Callback) :: #{
	type := Type,
	callback := Callback,
	threshold := non_neg_integer(),
	cooldown => non_neg_integer()
}.

-type alarm_num_connections() :: alarm(num_connections, fun((ref(), term(), pid(), [pid()]) -> any())).

-type trusted_proxy() ::
	{inet:ip4_address(), 0..32} |
	{inet:ip6_address(), 0..128}.
-export_type([trusted_proxy/0]).

-type transport_opts(SocketOpts) :: #{
	alarms => #{term() => alarm_num_connections()},
	connection_type => worker | supervisor,
	handshake_timeout => timeout(),
	logger => module(),
	max_connections => max_conns(),
	num_acceptors => pos_integer(),
	num_conns_sups => pos_integer(),
	num_listen_sockets => pos_integer(),
	post_listen_callback => fun((term()) -> ok | {error, term()}),
	shutdown => timeout() | brutal_kill,
	trusted_proxies => [trusted_proxy()],
	socket_opts => SocketOpts
}.
-export_type([transport_opts/1]).

-type ref() :: any().
-export_type([ref/0]).

-spec start_listener(ref(), module(), opts(), module(), any())
	-> supervisor:startchild_ret().
start_listener(Ref, Transport, TransOpts0, Protocol, ProtoOpts)
		when is_atom(Transport), is_atom(Protocol) ->
	TransOpts = normalize_opts(TransOpts0),
	_ = code:ensure_loaded(Transport),
	case {erlang:function_exported(Transport, name, 0), validate_transport_opts(TransOpts)} of
		{true, ok} ->
			ChildSpec = #{id => {ranch_listener_sup, Ref}, start => {ranch_listener_sup, start_link, [
				Ref, Transport, TransOpts, Protocol, ProtoOpts
			]}, type => supervisor},
			maybe_started(supervisor:start_child(ranch_sup, ChildSpec));
		{false, _} ->
			{error, {bad_transport, Transport}};
		{_, TransOptsError} ->
			TransOptsError
	end.

-spec normalize_opts(opts()) -> transport_opts(any()).
normalize_opts(Map) when is_map(Map) ->
	Map;
normalize_opts(Any) ->
	#{socket_opts => Any}.

-spec validate_transport_opts(transport_opts(any())) -> ok | {error, any()}.
validate_transport_opts(Opts) ->
	maps:fold(fun
		(Key, Value, ok) ->
			case validate_transport_opt(Key, Value, Opts) of
				true ->
					ok;
				false ->
					{error, {bad_option, Key}}
			end;
		(_, _, Acc) ->
			Acc
	end, ok, Opts).

-spec validate_transport_opt(any(), any(), transport_opts(any())) -> boolean().
validate_transport_opt(connection_type, worker, _) ->
	true;
validate_transport_opt(connection_type, supervisor, _) ->
	true;
validate_transport_opt(handshake_timeout, infinity, _) ->
	true;
validate_transport_opt(handshake_timeout, Value, _) ->
	is_integer(Value) andalso Value >= 0;
validate_transport_opt(max_connections, infinity, _) ->
	true;
validate_transport_opt(max_connections, Value, _) ->
	is_integer(Value) andalso Value >= 0;
validate_transport_opt(alarms, Alarms, _) ->
	Alarms1 = compat_normalize_alarms_option(Alarms),
	maps:fold(
		fun
			(_, Opts, true) ->
				validate_alarm(Opts);
			(_, _, false) ->
				false
		end,
		true,
		Alarms1);
validate_transport_opt(logger, Value, _) ->
	is_atom(Value);
validate_transport_opt(num_acceptors, Value, _) ->
	is_integer(Value) andalso Value > 0;
validate_transport_opt(num_conns_sups, Value, _) ->
	is_integer(Value) andalso Value > 0;
validate_transport_opt(num_listen_sockets, Value, Opts) ->
	is_integer(Value) andalso Value > 0
		andalso Value =< maps:get(num_acceptors, Opts, 10);
validate_transport_opt(post_listen_callback, Value, _) ->
	is_function(Value, 1);
validate_transport_opt(shutdown, brutal_kill, _) ->
	true;
validate_transport_opt(shutdown, infinity, _) ->
	true;
validate_transport_opt(shutdown, Value, _) ->
	is_integer(Value) andalso Value >= 0;
validate_transport_opt(socket_opts, _, _) ->
	true;
validate_transport_opt(trusted_proxies, Value, _) ->
	validate_trusted_proxies(Value);
validate_transport_opt(_, _, _) ->
	false.

validate_trusted_proxies([]) ->
	true;
validate_trusted_proxies([Proxy|Tail]) ->
	case validate_trusted_proxy(Proxy) of
		true ->
			validate_trusted_proxies(Tail);
		false ->
			false
	end;
validate_trusted_proxies(_) ->
	false.

%% IPv4-mapped IPv6 is not allowed.
validate_trusted_proxy({{0, 0, 0, 0, 0, 16#ffff, _, _}, _}) ->
	false;
validate_trusted_proxy({Addr, Len})
		when tuple_size(Addr) =:= 4, is_integer(Len),
			Len >= 0, Len =< 32 ->
	inet:is_ipv4_address(Addr);
validate_trusted_proxy({Addr, Len})
		when tuple_size(Addr) =:= 8, is_integer(Len),
			Len >= 0, Len =< 128 ->
	inet:is_ipv6_address(Addr);
validate_trusted_proxy(_) ->
	false.

validate_alarm(Alarm = #{type := num_connections, threshold := Threshold,
		callback := Callback}) ->
	is_integer(Threshold) andalso Threshold >= 0
	andalso is_function(Callback, 4)
	andalso case Alarm of
		#{cooldown := Cooldown} ->
			is_integer(Cooldown) andalso Cooldown >= 0;
		_ ->
			true
	end;
validate_alarm(_) ->
	false.

maybe_started({error, {{shutdown,
		{failed_to_start_child, ranch_acceptors_sup,
			{listen_error, _, Reason}}}, _}} = Error) ->
	start_error(Reason, Error);
maybe_started(Res) ->
	Res.

start_error(E=eaddrinuse, _) -> {error, E};
start_error(E=eacces, _) -> {error, E};
start_error(E=no_cert, _) -> {error, E};
start_error(_, Error) -> Error.

-spec stop_listener(ref()) -> ok | {error, not_found}.
stop_listener(Ref) ->
	%% The stop procedure must be executed in a separate
	%% process to make sure that it won't be interrupted
	%% in the middle in case the calling process crashes.
	%% We use erpc:call locally so we don't have to
	%% implement a custom spawn/call mechanism.
	%% We need to provide an integer timeout to erpc:call,
	%% otherwise the function will be executed in the calling
	%% process. 5 minutes should be enough.
	erpc:call(node(), fun() -> stop_listener1(Ref) end, 300000).

stop_listener1(Ref) ->
	TransportAndOpts = maybe_get_transport_and_opts(Ref),
	_ = supervisor:terminate_child(ranch_sup, {ranch_listener_sup, Ref}),
	ok = ranch_server:cleanup_listener_opts(Ref),
	Result = supervisor:delete_child(ranch_sup, {ranch_listener_sup, Ref}),
	ok = stop_listener2(TransportAndOpts),
	Result.

stop_listener2({Transport, TransOpts}) ->
	Transport:cleanup(TransOpts),
	ok;
stop_listener2(undefined) ->
	ok.

maybe_get_transport_and_opts(Ref) ->
	try
		[_, Transport, _, _, _] = ranch_server:get_listener_start_args(Ref),
		TransOpts = get_transport_options(Ref),
		{Transport, TransOpts}
	catch
		error:badarg ->
			undefined
	end.

-spec suspend_listener(ref()) -> ok | {error, any()}.
suspend_listener(Ref) ->
	case get_status(Ref) of
		running ->
			ListenerSup = ranch_server:get_listener_sup(Ref),
			ok = ranch_server:set_addr(Ref, {undefined, undefined}),
			supervisor:terminate_child(ListenerSup, ranch_acceptors_sup);
		suspended ->
			ok
	end.

-spec resume_listener(ref()) -> ok | {error, any()}.
resume_listener(Ref) ->
	case get_status(Ref) of
		running ->
			ok;
		suspended ->
			ListenerSup = ranch_server:get_listener_sup(Ref),
			Res = supervisor:restart_child(ListenerSup, ranch_acceptors_sup),
			maybe_resumed(Res)
	end.

maybe_resumed(Error={error, {listen_error, _, Reason}}) ->
	start_error(Reason, Error);
maybe_resumed({ok, _}) ->
	ok;
maybe_resumed({ok, _, _}) ->
	ok;
maybe_resumed(Res) ->
	Res.

-spec stop_all_acceptors() -> ok.
stop_all_acceptors() ->
	_ = [ok = do_acceptors(Pid, terminate_child)
		|| {_, Pid} <- ranch_server:get_listener_sups()],
	ok.

-spec restart_all_acceptors() -> ok.
restart_all_acceptors() ->
	_ = [ok = do_acceptors(Pid, restart_child)
		|| {_, Pid} <- ranch_server:get_listener_sups()],
	ok.

do_acceptors(ListenerSup, F) ->
	ListenerChildren = supervisor:which_children(ListenerSup),
	case lists:keyfind(ranch_acceptors_sup, 1, ListenerChildren) of
		{_, AcceptorsSup, _, _} when is_pid(AcceptorsSup) ->
			AcceptorChildren = supervisor:which_children(AcceptorsSup),
			_ = [ok = do_acceptor(AcceptorsSup, AcceptorId, F)
				|| {AcceptorId, _, _, _} <- AcceptorChildren],
			ok;
		%% A suspended listener has no acceptors supervisor.
		{_, undefined, _, _} ->
			ok;
		{_, Atom, _, _} ->
			{error, Atom}
	end.

do_acceptor(AcceptorsSup, AcceptorId, terminate_child) ->
	case supervisor:terminate_child(AcceptorsSup, AcceptorId) of
		ok ->
			ok;
		Error ->
			error(Error)
	end;
do_acceptor(AcceptorsSup, AcceptorId, restart_child) ->
	case supervisor:restart_child(AcceptorsSup, AcceptorId) of
		{ok, _} ->
			ok;
		{ok, _, _} ->
			ok;
		Error ->
			error(Error)
	end.

-spec child_spec(ref(), module(), opts(), module(), any())
	-> supervisor:child_spec().
child_spec(Ref, Transport, TransOpts0, Protocol, ProtoOpts) ->
	TransOpts = normalize_opts(TransOpts0),
	#{id => {ranch_embedded_sup, Ref}, start => {ranch_embedded_sup, start_link, [
		Ref, Transport, TransOpts, Protocol, ProtoOpts
	]}, type => supervisor}.

-spec handshake(ref()) -> {ok, ranch_transport:socket()} | {continue, any()}.
handshake(Ref) ->
	handshake1(Ref, undefined).

-spec handshake(ref(), any()) -> {ok, ranch_transport:socket()} | {continue, any()}.
handshake(Ref, Opts) ->
	handshake1(Ref, {opts, Opts}).

handshake1(Ref, Opts) ->
	receive {handshake, Ref, Transport, CSocket, Timeout} ->
		PeerInfo = get_peer_info(Transport, CSocket, undefined),
		Handshake = handshake_transport(Transport, handshake, CSocket, Opts, Timeout),
		handshake_result(Handshake, Ref, Transport, CSocket, PeerInfo, Timeout)
	end.

-spec handshake_continue(ref()) -> {ok, ranch_transport:socket()}.
handshake_continue(Ref) ->
	handshake_continue1(Ref, undefined).

-spec handshake_continue(ref(), any()) -> {ok, ranch_transport:socket()}.
handshake_continue(Ref, Opts) ->
	handshake_continue1(Ref, {opts, Opts}).

handshake_continue1(Ref, Opts) ->
	receive {handshake_continue, Ref, Transport, CSocket, Timeout} ->
		PeerInfo = get_peer_info(Transport, CSocket, undefined),
		Handshake = handshake_transport(Transport, handshake_continue, CSocket, Opts, Timeout),
		handshake_result(Handshake, Ref, Transport, CSocket, PeerInfo, Timeout)
	end.

handshake_transport(Transport, Fun, CSocket, undefined, Timeout) ->
	Transport:Fun(CSocket, Timeout);
handshake_transport(Transport, Fun, CSocket, {opts, Opts}, Timeout) ->
	Transport:Fun(CSocket, Opts, Timeout).

handshake_result(Result, Ref, Transport, CSocket, PeerInfo0, Timeout) ->
	case Result of
		OK = {ok, _} ->
			OK;
		{ok, CSocket2, Info} ->
			self() ! {handshake_continue, Ref, Transport, CSocket2, Timeout},
			{continue, Info};
		{error, Reason} ->
			PeerInfo = get_peer_info(Transport, CSocket, PeerInfo0),
			ok = Transport:close(CSocket),
			exit({shutdown, {Reason, PeerInfo}})
	end.

-spec handshake_cancel(ref()) -> ok.
handshake_cancel(Ref) ->
	receive {handshake_continue, Ref, Transport, CSocket, _} ->
		Transport:handshake_cancel(CSocket)
	end.

get_peer_info(Transport, Socket, Default) ->
	case Transport:peername(Socket) of
		{ok, Peer} -> Peer;
		{error, _} -> Default
	end.

%% Unlike handshake/2 this function always return errors because
%% the communication between the proxy and the server are expected
%% to be reliable. If there is a problem while receiving the proxy
%% header, we probably want to know about it.
-spec recv_proxy_header(ref(), timeout())
	-> {ok, ranch_proxy_header:proxy_info()}
	| {error, closed | atom()}
	| {error, protocol_error, atom()}.
recv_proxy_header(Ref, Timeout) ->
	receive HandshakeState={handshake, Ref, Transport, CSocket, _} ->
		self() ! HandshakeState,
		case check_proxy_peer(Ref, Transport, CSocket) of
			ok ->
				Transport:recv_proxy_header(CSocket, Timeout);
			Error ->
				Error
		end
	end.

check_proxy_peer(Ref, Transport, Socket) ->
	case ranch_server:get_transport_options(Ref) of
		#{trusted_proxies := Trusted} = TransOpts ->
			check_proxy_peer(Transport, Socket, Trusted,
				maps:get(logger, TransOpts, logger));
		_ ->
			ok
	end.

check_proxy_peer(Transport, Socket, Trusted, Logger) ->
	case Transport:peername(Socket) of
		{ok, Peer} ->
			case peer_trusted(Peer, Trusted) of
				true ->
					ok;
				false ->
					log(warning,
						"Ranch rejected a PROXY protocol connection "
						"from untrusted peer ~0p~n",
						[Peer], Logger),
					{error, protocol_error,
						'The PROXY protocol connection from an untrusted peer was rejected.'}
			end;
		{error, Reason} ->
			{error, Reason}
	end.

%% ::ffff:a.b.c.d is an IPv4 peer accepted on an IPv6 socket.
%% Compare it only to IPv4 entries.
peer_trusted({{A, B, C, D}, _}, Trusted) when is_integer(A) ->
	ipv4_trusted({A, B, C, D}, Trusted);
peer_trusted({{0, 0, 0, 0, 0, 16#ffff, AB, CD}, _}, Trusted)
		when is_integer(AB), is_integer(CD) ->
	ipv4_trusted({AB bsr 8, AB band 16#ff, CD bsr 8, CD band 16#ff},
		Trusted);
peer_trusted({{_, _, _, _, _, _, _, _} = Addr, _}, Trusted) ->
	ipv6_trusted(Addr, Trusted);
peer_trusted(_, _) ->
	false.

ipv4_trusted(Addr, Trusted) ->
	lists:any(fun (Entry) -> ipv4_cidr(Addr, Entry) end, Trusted).

ipv6_trusted(Addr, Trusted) ->
	lists:any(fun (Entry) -> ipv6_cidr(Addr, Entry) end, Trusted).

ipv4_cidr(Addr, {Net, Len}) when tuple_size(Net) =:= 4 ->
	Mask = cidr_mask(32, Len),
	(ip4_int(Addr) band Mask) =:= (ip4_int(Net) band Mask);
ipv4_cidr(_, _) ->
	false.

ipv6_cidr(Addr, {Net, Len}) when tuple_size(Net) =:= 8 ->
	Mask = cidr_mask(128, Len),
	(ip6_int(Addr) band Mask) =:= (ip6_int(Net) band Mask);
ipv6_cidr(_, _) ->
	false.

cidr_mask(Width, Len) ->
	((1 bsl Width) - 1) bxor ((1 bsl (Width - Len)) - 1).

ip4_int({A, B, C, D}) ->
	(A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D.

ip6_int({A, B, C, D, E, F, G, H}) ->
	(A bsl 112) bor (B bsl 96) bor (C bsl 80) bor (D bsl 64)
		bor (E bsl 48) bor (F bsl 32) bor (G bsl 16) bor H.

-spec remove_connection(ref()) -> ok.
remove_connection(Ref) ->
	remove_connection(Ref, self()).

-spec remove_connection(ref(), pid()) -> ok.
remove_connection(Ref, Pid) when is_pid(Pid) ->
	ListenerSup = ranch_server:get_listener_sup(Ref),
	{_, ConnsSupSup, _, _} = lists:keyfind(ranch_conns_sup_sup, 1,
		supervisor:which_children(ListenerSup)),
	_ = [ConnsSup ! {remove_connection, Ref, Pid} ||
		{_, ConnsSup, _, _} <- supervisor:which_children(ConnsSupSup)],
	ok.

-spec get_status(ref()) -> running | suspended.
get_status(Ref) ->
	ListenerSup = ranch_server:get_listener_sup(Ref),
	Children = supervisor:which_children(ListenerSup),
	case lists:keyfind(ranch_acceptors_sup, 1, Children) of
		{_, undefined, _, _} ->
			suspended;
		_ ->
			running
	end.

-spec get_addr(ref()) -> {inet:ip_address(), inet:port_number()} |
	{local, binary()} | {undefined, undefined}.
get_addr(Ref) ->
	ranch_server:get_addr(Ref).

-spec get_port(ref()) -> inet:port_number() | undefined.
get_port(Ref) ->
	case get_addr(Ref) of
		{local, _} ->
			undefined;
		{_, Port} ->
			Port
	end.

-spec get_connections(ref(), active|all) -> non_neg_integer().
get_connections(Ref, active) ->
	SupCounts = [ranch_conns_sup:active_connections(ConnsSup) ||
		{_, ConnsSup} <- ranch_server:get_connections_sups(Ref)],
	lists:sum(SupCounts);
get_connections(Ref, all) ->
	SupCounts = [proplists:get_value(active, supervisor:count_children(ConnsSup)) ||
		{_, ConnsSup} <- ranch_server:get_connections_sups(Ref)],
	lists:sum(SupCounts).

-spec get_max_connections(ref()) -> max_conns().
get_max_connections(Ref) ->
	ranch_server:get_max_connections(Ref).

-spec set_max_connections(ref(), max_conns()) -> ok.
set_max_connections(Ref, MaxConnections) ->
	ranch_server:set_max_connections(Ref, MaxConnections).

-spec get_transport_options(ref()) -> transport_opts(any()).
get_transport_options(Ref) ->
	ranch_server:get_transport_options(Ref).

-spec set_transport_options(ref(), opts()) -> ok | {error, term()}.
set_transport_options(Ref, TransOpts0) ->
	TransOpts = normalize_opts(TransOpts0),
	case validate_transport_opts(TransOpts) of
		ok ->
			ok = ranch_server:set_transport_options(Ref, TransOpts),
			ok = apply_transport_options(Ref, TransOpts);
		TransOptsError ->
			TransOptsError
	end.

apply_transport_options(Ref, TransOpts) ->
	%% A restarted connection supervisor reads this row.
	MaxConns = maps:get(max_connections, TransOpts, 1024),
	ok = ranch_server:set_max_connections(Ref, MaxConns),
	_ = [ConnsSup ! {set_transport_options, TransOpts}
		|| {_, ConnsSup} <- ranch_server:get_connections_sups(Ref)],
	ok.

-spec get_protocol_options(ref()) -> any().
get_protocol_options(Ref) ->
	ranch_server:get_protocol_options(Ref).

-spec set_protocol_options(ref(), any()) -> ok.
set_protocol_options(Ref, Opts) ->
	ranch_server:set_protocol_options(Ref, Opts).

-spec info() -> #{ref() := #{atom() := term()}}.
info() ->
	lists:foldl(
		fun ({Ref, Pid}, Acc) ->
			Acc#{Ref => listener_info(Ref, Pid)}
		end,
		#{},
		ranch_server:get_listener_sups()
	).

-spec info(ref()) -> #{atom() := term()}.
info(Ref) ->
	Pid = ranch_server:get_listener_sup(Ref),
	listener_info(Ref, Pid).

listener_info(Ref, Pid) ->
	[_, Transport, _, Protocol, _] = ranch_server:get_listener_start_args(Ref),
	Status = get_status(Ref),
	{IP, Port} = case get_addr(Ref) of
		Addr = {local, _} ->
			{Addr, undefined};
		Addr ->
			Addr
	end,
	MaxConns = get_max_connections(Ref),
	TransOpts = ranch_server:get_transport_options(Ref),
	ProtoOpts = get_protocol_options(Ref),
	#{
		pid => Pid,
		status => Status,
		ip => IP,
		port => Port,
		max_connections => MaxConns,
		active_connections => get_connections(Ref, active),
		all_connections => get_connections(Ref, all),
		transport => Transport,
		transport_options => TransOpts,
		protocol => Protocol,
		protocol_options => ProtoOpts,
		metrics => metrics(Ref)
	}.

-spec procs(ref(), acceptors | connections) ->
	[pid() | undefined | restarting].
procs(Ref, Type) ->
	ListenerSup = ranch_server:get_listener_sup(Ref),
	procs1(ListenerSup, Type).

procs1(ListenerSup, acceptors) ->
	{_, SupPid, _, _} = lists:keyfind(ranch_acceptors_sup, 1,
		supervisor:which_children(ListenerSup)),
	try
		[Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid)]
	catch exit:{noproc, _} ->
		[]
	end;
procs1(ListenerSup, connections) ->
	{_, SupSupPid, _, _} = lists:keyfind(ranch_conns_sup_sup, 1,
		supervisor:which_children(ListenerSup)),
	Conns=
	lists:map(fun ({_, SupPid, _, _}) ->
			[Pid || {_, Pid, _, _} <- supervisor:which_children(SupPid)]
		end,
		supervisor:which_children(SupSupPid)
	),
	lists:flatten(Conns).

-spec trace_acceptors(ref()) ->
	#{elapsed := non_neg_integer(), errors := [{pid(), term()}]}.
trace_acceptors(Ref) ->
	trace_acceptors(Ref, #{}).

-spec trace_acceptors(ref(), map()) ->
	#{elapsed := non_neg_integer(), errors := [{pid(), term()}]}.
trace_acceptors(_, #{limit := Limit})
		when not is_integer(Limit); Limit < 1 ->
	error(badarg);
trace_acceptors(_, #{timeout := Timeout})
		when not is_integer(Timeout);
			not (Timeout >= 0 andalso Timeout =< 16#ffffffff) ->
	error(badarg);
trace_acceptors(Ref, Opts) when is_map(Opts) ->
	trace_acceptors1(Ref,
		maps:get(limit, Opts, 5),
		maps:get(timeout, Opts, 1000)).

trace_acceptors1(Ref, Limit, Timeout) ->
	case [Pid || Pid <- procs(Ref, acceptors), is_pid(Pid)] of
		%% No acceptor is running.
		[] ->
			#{elapsed => 0, errors => []};
		Pids ->
			[_, Transport, _, _, _] = ranch_server:get_listener_start_args(Ref),
			trace_acceptors_run(Transport, Pids, Limit, Timeout)
	end.

trace_acceptors_run(Transport, Pids, Limit, Timeout) ->
	Parent = self(),
	{Tracer, MonRef} = spawn_monitor(fun() ->
		trace_acceptors_tracer(Parent)
	end),
	try
		Session = trace:session_create(ranch_trace_acceptors, Tracer, []),
		try
			_ = trace:function(Session, {Transport, accept, 2},
				[{'_', [], [{message, false}, {return_trace}]}], []),
			%% The pid may have exited between the snapshot and this call.
			_ = [try
				trace:process(Session, Pid, true, [call, arity])
			catch error:badarg ->
				0
			end || Pid <- Pids],
			_ = Tracer ! {start, Parent, Limit, Timeout, Session, Transport, Pids},
			receive
				{Tracer, Result} ->
					Result;
				{'DOWN', MonRef, process, Tracer, Reason} ->
					exit(Reason)
			end
		after
			_ = trace:session_destroy(Session)
		end
	after
		_ = exit(Tracer, kill),
		_ = erlang:demonitor(MonRef, [flush])
	end.

trace_acceptors_tracer(Parent) ->
	Mon = erlang:monitor(process, Parent),
	receive
		{start, Parent, Limit, Timeout, Session, Transport, Pids} ->
			Start = erlang:monotonic_time(microsecond),
			Deadline = erlang:monotonic_time(millisecond) + Timeout,
			trace_acceptors_collect(Parent, Mon, Limit, Start, Deadline,
				Session, Transport, Pids, []);
		{'DOWN', Mon, process, Parent, _} ->
			ok
	end.

trace_acceptors_collect(Parent, Mon, 0, Start, _Deadline,
		Session, Transport, Pids, Errors) ->
	trace_acceptors_finish(Parent, Mon, 0, Start, Session, Transport,
		Pids, Errors);
trace_acceptors_collect(Parent, Mon, Limit, Start, Deadline,
		Session, Transport, Pids, Errors) ->
	Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
	case Wait of
		0 ->
			trace_acceptors_finish(Parent, Mon, Limit, Start, Session,
				Transport, Pids, Errors);
		_ ->
			receive
				{trace, Pid, return_from, {_, accept, 2}, {error, Reason}} ->
					trace_acceptors_collect(Parent, Mon, Limit - 1, Start,
						Deadline, Session, Transport, Pids,
						[{Pid, Reason}|Errors]);
				{trace, _, call, {_, accept, _}} ->
					trace_acceptors_collect(Parent, Mon, Limit, Start,
						Deadline, Session, Transport, Pids, Errors);
				{trace, _, return_from, {_, accept, 2}, _} ->
					trace_acceptors_collect(Parent, Mon, Limit, Start,
						Deadline, Session, Transport, Pids, Errors);
				{'DOWN', Mon, process, Parent, _} ->
					ok
			after Wait ->
				trace_acceptors_finish(Parent, Mon, Limit, Start, Session,
					Transport, Pids, Errors)
			end
	end.

trace_acceptors_finish(Parent, Mon, Limit, Start, Session, Transport,
		Pids, Errors) ->
	_ = trace:function(Session, {Transport, accept, 2}, false, []),
	_ = [try
		trace:process(Session, Pid, false, [call, arity])
	catch error:badarg ->
		0
	end || Pid <- Pids],
	trace_acceptors_drain(Parent, Mon, Limit, Start, Errors).

%% Tracing is stopped. Take errors until limit, then reply.
trace_acceptors_drain(Parent, _Mon, 0, Start, Errors) ->
	trace_acceptors_reply(Parent, Start, Errors);
trace_acceptors_drain(Parent, Mon, Limit, Start, Errors) ->
	receive
		{trace, Pid, return_from, {_, accept, 2}, {error, Reason}} ->
			trace_acceptors_drain(Parent, Mon, Limit - 1, Start,
				[{Pid, Reason}|Errors]);
		{'DOWN', Mon, process, Parent, _} ->
			ok
	after 0 ->
		trace_acceptors_reply(Parent, Start, Errors)
	end.

trace_acceptors_reply(Parent, Start, Errors) ->
	%% elapsed 0 is reserved for the no-acceptor result. The clock
	%% can report the same microsecond twice.
	Elapsed = max(1, erlang:monotonic_time(microsecond) - Start),
	_ = Parent ! {self(), #{elapsed => Elapsed, errors => lists:reverse(Errors)}},
	ok.

-spec metrics(ref()) -> #{}.
metrics(Ref) ->
	Counters = ranch_server:get_stats_counters(Ref),
	CounterInfo = counters:info(Counters),
	NumCounters = maps:get(size, CounterInfo),
	NumConnsSups = NumCounters div 2,
	lists:foldl(
		fun (Id, Acc) ->
			Acc#{
				{conns_sup, Id, accept} => counters:get(Counters, 2*Id-1),
				{conns_sup, Id, terminate} => counters:get(Counters, 2*Id)
			}
		end,
		#{},
		lists:seq(1, NumConnsSups)
	).

-spec wait_for_connections
	(ref(), '>' | '>=' | '==' | '=<', non_neg_integer()) -> ok;
	(ref(), '<', pos_integer()) -> ok.
wait_for_connections(Ref, Op, NumConns) ->
	wait_for_connections(Ref, Op, NumConns, 1000).

-spec wait_for_connections
	(ref(), '>' | '>=' | '==' | '=<', non_neg_integer(), non_neg_integer()) -> ok;
	(ref(), '<', pos_integer(), non_neg_integer()) -> ok.
wait_for_connections(Ref, Op, NumConns, Interval) ->
	validate_op(Op, NumConns),
	validate_num_conns(NumConns),
	validate_interval(Interval),
	wait_for_connections_loop(Ref, Op, NumConns, Interval).

validate_op('>', _) -> ok;
validate_op('>=', _) -> ok;
validate_op('==', _) -> ok;
validate_op('=<', _) -> ok;
validate_op('<', NumConns) when NumConns > 0 -> ok;
validate_op(_, _) -> error(badarg).

validate_num_conns(NumConns) when is_integer(NumConns), NumConns >= 0 -> ok;
validate_num_conns(_) -> error(badarg).

validate_interval(Interval) when is_integer(Interval), Interval >= 0 -> ok;
validate_interval(_) -> error(badarg).

wait_for_connections_loop(Ref, Op, NumConns, Interval) ->
	CurConns = try
		get_connections(Ref, all)
	catch _:_ ->
		0
	end,
	case erlang:Op(CurConns, NumConns) of
		true ->
			ok;
		false when Interval =:= 0 ->
			wait_for_connections_loop(Ref, Op, NumConns, Interval);
		false ->
			timer:sleep(Interval),
			wait_for_connections_loop(Ref, Op, NumConns, Interval)
	end.

-spec filter_options([inet | inet6 | {atom(), any()} | {raw, any(), any(), any()}],
	[atom()], Acc, module()) -> Acc when Acc :: [any()].
filter_options(UserOptions, DisallowedKeys, DefaultOptions, Logger) ->
	AllowedOptions = filter_user_options(UserOptions, DisallowedKeys, Logger),
	lists:foldl(fun merge_options/2, DefaultOptions, AllowedOptions).

%% 2-tuple options.
filter_user_options([Opt = {Key, _}|Tail], DisallowedKeys, Logger) ->
	case lists:member(Key, DisallowedKeys) of
		false ->
			[Opt|filter_user_options(Tail, DisallowedKeys, Logger)];
		true ->
			filter_options_warning(Opt, Logger),
			filter_user_options(Tail, DisallowedKeys, Logger)
	end;
%% Special option forms.
filter_user_options([inet|Tail], DisallowedKeys, Logger) ->
	[inet|filter_user_options(Tail, DisallowedKeys, Logger)];
filter_user_options([inet6|Tail], DisallowedKeys, Logger) ->
	[inet6|filter_user_options(Tail, DisallowedKeys, Logger)];
filter_user_options([Opt = {raw, _, _, _}|Tail], DisallowedKeys, Logger) ->
	[Opt|filter_user_options(Tail, DisallowedKeys, Logger)];
filter_user_options([Opt|Tail], DisallowedKeys, Logger) ->
	filter_options_warning(Opt, Logger),
	filter_user_options(Tail, DisallowedKeys, Logger);
filter_user_options([], _, _) ->
	[].

filter_options_warning(Opt, Logger) ->
	log(warning,
		"Transport option ~p unknown or invalid.~n",
		[Opt], Logger).

merge_options({Key, _} = Option, OptionList) ->
	lists:keystore(Key, 1, OptionList, Option);
merge_options(Option, OptionList) ->
	[Option|OptionList].

-spec set_option_default(Opts, atom(), any())
	-> Opts when Opts :: [{atom(), any()}].
set_option_default(Opts, Key, Value) ->
	case lists:keymember(Key, 1, Opts) of
		true -> Opts;
		false -> [{Key, Value}|Opts]
	end.

-spec require([atom()]) -> ok.
require([]) ->
	ok;
require([App|Tail]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Tail).

-spec log(logger:level(), io:format(), list(), module() | #{logger => module()}) -> ok.
log(Level, Format, Args, Logger) when is_atom(Logger) ->
	log(Level, Format, Args, #{logger => Logger});
log(Level, Format, Args, #{logger := Logger})
		when Logger =/= error_logger ->
	_ = Logger:Level(Format, Args),
	ok;
%% Because error_logger does not have all the levels
%% we accept we have to do some mapping to error_logger functions.
log(Level, Format, Args, _) ->
	Function = case Level of
		emergency -> error_msg;
		alert -> error_msg;
		critical -> error_msg;
		error -> error_msg;
		warning -> warning_msg;
		notice -> warning_msg;
		info -> info_msg;
		debug -> info_msg
	end,
	error_logger:Function(Format, Args).

%% For backwards compatibility with the misspelled alarm
%% setting `treshold`.
%% See https://github.com/ninenines/ranch/issues/349
-spec compat_normalize_alarms_option(any()) -> any().
compat_normalize_alarms_option(Alarms = #{}) ->
	maps:map(
		fun
			(_, Alarm = #{threshold := _}) ->
				maps:remove(treshold, Alarm);
			(_, Alarm = #{treshold := Threshold}) ->
				maps:put(threshold, Threshold, maps:remove(treshold, Alarm));
			(_, Alarm) ->
				Alarm
		end,
		Alarms
	);
compat_normalize_alarms_option(Alarms) ->
	Alarms.
