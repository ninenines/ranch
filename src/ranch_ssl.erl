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

-module(ranch_ssl).
-behaviour(ranch_transport).

-include_lib("kernel/include/file.hrl").

-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
-export([disallowed_listen_options/0]).
-export([accept/2]).
-export([handshake/2]).
-export([handshake/3]).
-export([handshake_continue/2]).
-export([handshake_continue/3]).
-export([handshake_cancel/1]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([recv_proxy_header/2]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([getopts/2]).
-export([getstat/1]).
-export([getstat/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([peercert/1]).
-export([negotiated_protocol/1]).
-export([shutdown/2]).
-export([close/1]).
-export([cleanup/1]).
-export([format_error/1]).

-type ssl_opt() :: {alpn_preferred_protocols, [binary()]}
	| {anti_replay, '10k' | '100k' | {integer(), integer(), integer()}}
	| {beast_mitigation, one_n_minus_one | zero_n | disabled}
	| {cacertfile, file:filename()}
	| {cacerts, [public_key:der_encoded()] | [public_key:combined_cert()]}
	| {cert, public_key:der_encoded() | [public_key:der_encoded()]}
	| {certs_keys, [#{cert => public_key:der_encoded()
				| [public_key:der_encoded()],
			  key => ssl:key(),
			  certfile => file:filename(),
			  keyfile => file:filename(),
			  password => iodata() | fun(() -> iodata())}]}
	| {certfile, file:filename()}
	| {ciphers, ssl:ciphers()}
	| {client_renegotiation, boolean()}
	| {crl_cache, {atom(), {internal | term(), [any()]}}}
	| {crl_check, boolean() | peer | best_effort}
	| {depth, integer()}
	| {dh, binary()}
	| {dhfile, file:filename()}
	| {eccs, [ssl:named_curve()]}
	| {fail_if_no_peer_cert, boolean()}
	| {handshake, hello | full}
	| {hibernate_after, timeout()}
	| {honor_cipher_order, boolean()}
	| {honor_ecc_order, boolean()}
	| {key, ssl:key()}
	| {key_update_at, pos_integer()}
	| {keyfile, file:filename()}
	| {log_alert, boolean()}
	| {log_level, logger:level() | none | all}
	| {max_handshake_size, integer()}
	| {middlebox_comp_mode, boolean()}
	| {next_protocols_advertised, [binary()]}
	| {padding_check, boolean()}
	| {partial_chain, fun()}
	| {password, iodata() | fun(() -> iodata())}
	| {protocol, tls | dtls}
	| {psk_identity, string()}
	| {reuse_session, fun()}
	| {reuse_sessions, boolean()}
	| {secure_renegotiate, boolean()}
	| {session_tickets, disabled | stateful | stateless
		| stateful_with_cert | stateless_with_cert}
	| {signature_algs, ssl:signature_algs()}
	| {signature_algs_cert, [ssl:sign_scheme()]}
	| {sni_fun, fun()}
	| {sni_hosts, [{inet:hostname(), [ssl_opt()]}]}
	| {stateless_tickets_seed, binary()}
	| {supported_groups, [ssl:group()]}
	| {user_lookup_fun, {fun(), any()}}
	| {verify, verify_none | verify_peer}
	| {verify_fun, {fun(), any()}}
	| {versions, [ssl:protocol_version()]}.
-export_type([ssl_opt/0]).

-type opt() :: ranch_tcp:opt() | ssl_opt().
-export_type([opt/0]).

-type opts() :: [opt()].
-export_type([opts/0]).

-spec name() -> ssl.
name() -> ssl.

-spec secure() -> boolean().
secure() ->
	true.

-spec messages() -> {ssl, ssl_closed, ssl_error, ssl_passive}.
messages() -> {ssl, ssl_closed, ssl_error, ssl_passive}.

-spec listen(ranch:transport_opts(opts())) -> {ok, ssl:sslsocket()} | {error, atom()}.
listen(TransOpts) ->
	ok = cleanup(TransOpts),
	SocketOpts = maps:get(socket_opts, TransOpts, []),
	case lists:keymember(cert, 1, SocketOpts)
			orelse lists:keymember(certfile, 1, SocketOpts)
			orelse lists:keymember(sni_fun, 1, SocketOpts)
			orelse lists:keymember(sni_hosts, 1, SocketOpts)
			orelse lists:keymember(user_lookup_fun, 1, SocketOpts)
			orelse lists:keymember(certs_keys, 1, SocketOpts) of
		true ->
			Logger = maps:get(logger, TransOpts, logger),
			do_listen(SocketOpts, Logger);
		false ->
			{error, no_cert}
	end.

do_listen(SocketOpts0, Logger) ->
	SocketOpts = prepare_socket_opts(SocketOpts0, Logger),
	case proplists:get_value(protocol, SocketOpts, tls) of
		dtls ->
			dtls_listen(SocketOpts);
		tls ->
			%% The port in the options takes precedence over the
			%% first argument.
			ssl:listen(0, SocketOpts)
	end.

%% OTP monitors the process that called ssl:listen/2. That process
%% is ranch_acceptors_sup, and suspend stops it. A fixed-port DTLS
%% socket is owned here instead, so the UDP port stays open for
%% connections that are still up. The next listen closes and listens
%% again: close drops acceptors that died with the previous
%% supervisor, and the real port finds the demux that is still
%% registered. The first argument stays 0 for a new socket so a
%% second listen (num_listen_sockets) does not take the same demux.
-define(DTLS_TAB, ranch_ssl_dtls).

dtls_listen(SocketOpts) ->
	case proplists:get_value(port, SocketOpts, 0) of
		0 ->
			ssl:listen(0, SocketOpts);
		Port ->
			IP = proplists:get_value(ip, SocketOpts, dtls_any_ip(SocketOpts)),
			dtls_reuse_listen({IP, Port}, SocketOpts)
	end.

dtls_any_ip(SocketOpts) ->
	case proplists:get_value(inet6, SocketOpts, false) of
		true ->
			{0, 0, 0, 0, 0, 0, 0, 0};
		false ->
			{0, 0, 0, 0}
	end.

dtls_reuse_listen(Key, SocketOpts) ->
	case ets:lookup(dtls_tab(), Key) of
		[{_, Holder}] ->
			case dtls_claim(Holder, SocketOpts) of
				{ok, _}=OK ->
					OK;
				again ->
					%% The holder died and left its registration behind.
					ets:delete_object(dtls_tab(), {Key, Holder}),
					dtls_fresh(Key, SocketOpts);
				busy ->
					ssl:listen(0, SocketOpts)
			end;
		[] ->
			dtls_fresh(Key, SocketOpts)
	end.

dtls_fresh(Key, SocketOpts) ->
	Parent = self(),
	Holder = spawn(fun() -> dtls_holder_init(Key, SocketOpts, Parent) end),
	Mon = monitor(process, Holder),
	receive
		{Holder, {ok, Socket}} ->
			demonitor(Mon, [flush]),
			{ok, Socket};
		{Holder, again} ->
			demonitor(Mon, [flush]),
			dtls_reuse_listen(Key, SocketOpts);
		{Holder, Error} ->
			demonitor(Mon, [flush]),
			Error;
		{'DOWN', Mon, process, _, _} ->
			{error, closed}
	after 5000 ->
		demonitor(Mon, [flush]),
		exit(Holder, kill),
		{error, timeout}
	end.

dtls_claim(Holder, SocketOpts) ->
	Ref = make_ref(),
	Mon = monitor(process, Holder),
	Holder ! {claim, self(), Ref, SocketOpts},
	receive
		{Ref, Reply} ->
			demonitor(Mon, [flush]),
			Reply;
		{'DOWN', Mon, process, _, _} ->
			again
	after 5000 ->
		demonitor(Mon, [flush]),
		{error, timeout}
	end.

dtls_holder_init(Key, SocketOpts, User) ->
	case dtls_open(SocketOpts) of
		{ok, Socket} ->
			case ets:insert_new(dtls_tab(), {Key, self()}) of
				true ->
					Parent = dtls_parent(User),
					UserMon = monitor(process, User),
					ParentMon = dtls_monitor(Parent),
					User ! {self(), {ok, Socket}},
					dtls_holder(Key, Socket, User, UserMon, Parent, ParentMon);
				false ->
					_ = ssl:close(Socket),
					User ! {self(), again}
			end;
		Error ->
			User ! {self(), Error}
	end.

%% Stop leaves the demux registered until this process closes it.
dtls_open(SocketOpts) ->
	dtls_open(fun() -> ssl:listen(0, SocketOpts) end, 20).

dtls_open(Listen, 0) ->
	Listen();
dtls_open(Listen, N) ->
	case Listen() of
		{error, already_listening} ->
			receive after 10 -> ok end,
			dtls_open(Listen, N - 1);
		Other ->
			Other
	end.

dtls_reclaim(Socket, SocketOpts) ->
	_ = ssl:close(Socket),
	Port = proplists:get_value(port, SocketOpts, 0),
	dtls_open(fun() -> ssl:listen(Port, SocketOpts) end, 20).

dtls_holder(Key, Socket, User, UserMon, Parent, ParentMon) ->
	receive
		{claim, From, Ref, SocketOpts} ->
			case dtls_alive(User) of
				true ->
					From ! {Ref, busy},
					dtls_holder(Key, Socket, User, UserMon, Parent, ParentMon);
				false ->
					%% The acceptors supervisor is already gone. Its
					%% DOWN may still be sitting in the mailbox.
					_ = dtls_demonitor(UserMon),
					dtls_holder_claim(Key, Socket, From, Ref, SocketOpts,
						Parent, ParentMon)
			end;
		{'DOWN', UserMon, process, User, _} ->
			case dtls_alive(Parent) of
				true ->
					dtls_holder(Key, Socket, undefined, UserMon, Parent, ParentMon);
				false ->
					dtls_holder_stop(Key, Socket)
			end;
		{'DOWN', ParentMon, process, Parent, _} when User =:= undefined ->
			dtls_holder_stop(Key, Socket);
		{'DOWN', ParentMon, process, Parent, _} ->
			dtls_holder(Key, Socket, User, UserMon, undefined, undefined)
	end.

dtls_holder_claim(Key, Socket, From, Ref, SocketOpts, Parent, ParentMon) ->
	case dtls_reclaim(Socket, SocketOpts) of
		{ok, Socket1} ->
			From ! {Ref, {ok, Socket1}},
			Parent1 = dtls_parent(From),
			ParentMon1 = case Parent1 of
				Parent ->
					ParentMon;
				_ ->
					_ = dtls_demonitor(ParentMon),
					dtls_monitor(Parent1)
			end,
			dtls_holder(Key, Socket1, From, monitor(process, From),
				Parent1, ParentMon1);
		Error ->
			ets:delete(dtls_tab(), Key),
			From ! {Ref, Error}
	end.

dtls_demonitor(undefined) ->
	ok;
dtls_demonitor(Mon) ->
	demonitor(Mon, [flush]).

dtls_holder_stop(Key, Socket) ->
	ets:delete(dtls_tab(), Key),
	_ = ssl:close(Socket),
	ok.

dtls_parent(User) ->
	case process_info(User, parent) of
		{parent, Pid} when is_pid(Pid) ->
			Pid;
		_ ->
			undefined
	end.

dtls_monitor(undefined) ->
	undefined;
dtls_monitor(Pid) ->
	monitor(process, Pid).

dtls_alive(undefined) ->
	false;
dtls_alive(Pid) ->
	is_process_alive(Pid).

dtls_tab() ->
	case ets:whereis(?DTLS_TAB) of
		undefined ->
			Heir = case whereis(ranch_server) of
				undefined ->
					[];
				Pid ->
					[{heir, Pid, undefined}]
			end,
			try ets:new(?DTLS_TAB, [named_table, public, set|Heir])
			catch error:badarg ->
				?DTLS_TAB
			end;
		Tab ->
			Tab
	end.

prepare_socket_opts([Backend = {inet_backend, _}|SocketOpts], Logger) ->
	%% ssl conses non-emulated socket options. Before ssl 11.6
	%% that list is not reversed, so gen_tcp sees the last option
	%% first. ssl 11.6+ reverses it. gen_tcp exits badarg unless
	%% the tuple is first.
	Opts = prepare_socket_opts(SocketOpts, Logger),
	case ssl_preserves_socket_opt_order() of
		true ->
			[Backend|Opts];
		false ->
			Opts ++ [Backend]
	end;
prepare_socket_opts(SocketOpts0, Logger) ->
	SocketOpts = set_default_options(SocketOpts0),
	{DisallowedOpts0, Defaults} =
		case proplists:get_value(protocol, SocketOpts, tls) of
			%% SO_REUSEADDR on UDP lets another socket bind the same
			%% address and port and receive datagrams.
			dtls ->
				{disallowed_listen_options() -- [reuseaddr],
					[binary, {active, false}]};
			tls ->
				{disallowed_listen_options(),
					[binary, {active, false}, {reuseaddr, true}]}
		end,
	DisallowedOpts = unsupported_tls_options(SocketOpts) ++ DisallowedOpts0,
	ranch:filter_options(SocketOpts, DisallowedOpts, Defaults, Logger).

ssl_preserves_socket_opt_order() ->
	{ok, Vsn} = application:get_key(ssl, vsn),
	Ns = [list_to_integer(N) || N <- string:split(Vsn, ".", all)],
	Ns >= [11, 6].

set_default_options(SocketOpts0) ->
	case proplists:get_value(protocol, SocketOpts0, tls) of
		tls ->
			SocketOpts1 = ranch:set_option_default(SocketOpts0, backlog, 1024),
			SocketOpts2 = ranch:set_option_default(SocketOpts1, nodelay, true),
			SocketOpts3 = ranch:set_option_default(SocketOpts2, send_timeout, 30000),
			ranch:set_option_default(SocketOpts3, send_timeout_close, true);
		dtls ->
			SocketOpts0
	end.

%% 'binary' and 'list' are disallowed but they are handled
%% specifically as they do not have 2-tuple equivalents.
-spec disallowed_listen_options() -> [atom()].
disallowed_listen_options() ->
	[alpn_advertised_protocols, client_preferred_next_protocols,
		fallback, server_name_indication, srp_identity
		|ranch_tcp:disallowed_listen_options()].

unsupported_tls_options(SocketOpts) ->
	unsupported_tls_version_options(lists:usort(get_tls_versions(SocketOpts))).

unsupported_tls_version_options([tlsv1|_]) ->
	[];
unsupported_tls_version_options(['tlsv1.1'|_]) ->
	[beast_mitigation, padding_check];
unsupported_tls_version_options(['tlsv1.2'|_]) ->
	[beast_mitigation, padding_check];
unsupported_tls_version_options(['tlsv1.3'|_]) ->
	[beast_mitigation, client_renegotiation, next_protocols_advertised,
		padding_check, psk_identity, reuse_session, reuse_sessions,
		secure_renegotiate, user_lookup_fun];
unsupported_tls_version_options(_) ->
	[].

-spec accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	ssl:transport_accept(LSocket, Timeout).

-spec handshake(inet:socket() | ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {ok, ssl:sslsocket(), ssl:protocol_extensions()} | {error, any()}.
handshake(CSocket, Timeout) ->
	handshake(CSocket, [], Timeout).

-spec handshake(inet:socket() | ssl:sslsocket(), opts(), timeout())
	-> {ok, ssl:sslsocket()} | {ok, ssl:sslsocket(), ssl:protocol_extensions()} | {error, any()}.
handshake(CSocket, Opts, Timeout) ->
	case ssl:handshake(CSocket, Opts, Timeout) of
		OK = {ok, _} ->
			OK;
		OK = {ok, _, _} ->
			OK;
		Error = {error, _} ->
			Error
	end.

-spec handshake_continue(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, any()}.
handshake_continue(CSocket, Timeout) ->
	handshake_continue(CSocket, [], Timeout).

-spec handshake_continue(ssl:sslsocket(), [ssl:tls_server_option()], timeout())
	-> {ok, ssl:sslsocket()} | {error, any()}.
handshake_continue(CSocket, Opts, Timeout) ->
	case ssl:handshake_continue(CSocket, Opts, Timeout) of
		OK = {ok, _} ->
			OK;
		Error = {error, _} ->
			Error
	end.

-spec handshake_cancel(ssl:sslsocket()) -> ok.
handshake_cancel(CSocket) ->
	ok = ssl:handshake_cancel(CSocket).

-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}]).

-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}],
		Timeout).

-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

-spec recv_proxy_header(ssl:sslsocket(), timeout())
	-> {ok, ranch_proxy_header:proxy_info()}
	| {error, closed | atom()}
	| {error, protocol_error, atom()}.
recv_proxy_header(SSLSocket, Timeout) ->
	%% There's currently no documented way to perform a TCP recv
	%% on an sslsocket(), even before the TLS handshake. However
	%% nothing prevents us from retrieving the TCP socket and using
	%% it. Since it's an undocumented interface this may however
	%% make forward-compatibility more difficult.
	TCPSocket = case element(2, SSLSocket) of
		%% Before OTP-28.
		{gen_tcp, TCPSocket0, _, _} -> TCPSocket0;
		%% OTP-28+.
		TCPSocket0 -> TCPSocket0
	end,
	ranch_tcp:recv_proxy_header(TCPSocket, Timeout).

-spec send(ssl:sslsocket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) ->
	sendfile(Socket, Filename, 0, 0, []).

-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd(),
		non_neg_integer(), non_neg_integer())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
	sendfile(Socket, File, Offset, Bytes, []).

%% Unlike with TCP, no syscall can be used here, so sending files
%% through SSL will be much slower in comparison. Note that unlike
%% file:sendfile/5 this function accepts either a file or a file name.
-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd(),
		non_neg_integer(), non_neg_integer(), ranch_transport:sendfile_opts())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes, Opts) ->
	ranch_transport:sendfile(?MODULE, Socket, File, Offset, Bytes, Opts).

-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

-spec getopts(ssl:sslsocket(), [atom()]) -> {ok, list()} | {error, atom()}.
getopts(Socket, Opts) ->
	ssl:getopts(Socket, Opts).

-spec getstat(ssl:sslsocket()) -> {ok, list()} | {error, atom()}.
getstat(Socket) ->
	ssl:getstat(Socket).

-spec getstat(ssl:sslsocket(), [atom()]) -> {ok, list()} | {error, atom()}.
getstat(Socket, OptionNames) ->
	ssl:getstat(Socket, OptionNames).

-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

-spec sockname(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
sockname(Socket) ->
	ssl:sockname(Socket).

-spec peercert(ssl:sslsocket())
	-> {ok, public_key:der_encoded()} | {error, any()}.
peercert(Socket) ->
	ssl:peercert(Socket).

-spec negotiated_protocol(ssl:sslsocket())
	-> {ok, binary()} | {error, protocol_not_negotiated | closed}.
negotiated_protocol(Socket) ->
	ssl:negotiated_protocol(Socket).

-spec shutdown(ssl:sslsocket(), read | write | read_write)
	-> ok | {error, atom()}.
shutdown(Socket, How) ->
	ssl:shutdown(Socket, How).

-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

-spec cleanup(ranch:transport_opts(opts())) -> ok.
cleanup(#{socket_opts:=SocketOpts}) ->
	%% Only a socket (or FIFO) has type other. A regular file,
	%% directory, symlink, or device is left in place.
	maybe
		{ip, {local, SockFile}} ?= lists:keyfind(ip, 1, lists:reverse(SocketOpts)),
		{ok, #file_info{type = other}} ?= file:read_link_info(SockFile),
		ok ?= file:delete(SockFile)
	else
		_ ->
			ok
	end;
cleanup(_) ->
	ok.

-spec format_error({error, ssl:reason()} | ssl:reason()) -> string().
format_error(no_cert) ->
	"no certificate provided; see cert, certfile, certs_keys, sni_fun, sni_hosts or user_lookup_fun options";
format_error(Reason) ->
	ssl:format_error(Reason).

get_tls_versions(SocketOpts) ->
	%% Socket options need to be reversed for keyfind because later options
	%% take precedence when contained multiple times, but keyfind will return
	%% the earliest occurence.
	case lists:keyfind(versions, 1, lists:reverse(SocketOpts)) of
		{versions, Versions} ->
			Versions;
		false ->
			get_tls_versions_env()
	end.

get_tls_versions_env() ->
	case application:get_env(ssl, protocol_version) of
		{ok, Versions} ->
			Versions;
		undefined ->
			get_tls_versions_app()
	end.

get_tls_versions_app() ->
	{supported, Versions} = lists:keyfind(supported, 1, ssl:versions()),
	Versions.
