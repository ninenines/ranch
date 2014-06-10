%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
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

-export([name/0]).
-export([messages/0]).
-export([listen/1]).
-export([accept/2]).
-export([accept_ack/2]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).

-type opts() :: [{backlog, non_neg_integer()}
	| {cacertfile, string()}
	| {cacerts, [Der::binary()]}
	| {cert, Der::binary()}
	| {certfile, string()}
	| {ciphers, [ssl:erl_cipher_suite()] | string()}
	| {fail_if_no_peer_cert, boolean()}
	| {hibernate_after, integer() | undefined}
	| {honor_cipher_order, boolean()}
	| {ip, inet:ip_address()}
	| {key, Der::binary()}
	| {keyfile, string()}
	| {linger, {boolean(), non_neg_integer()}}
	| {log_alert, boolean()}
	| {next_protocols_advertised, [binary()]}
	| {nodelay, boolean()}
	| {password, string()}
	| {port, inet:port_number()}
	| {raw, non_neg_integer(), non_neg_integer(),
		non_neg_integer() | binary()}
	| {reuse_session, fun()}
	| {reuse_sessions, boolean()}
	| {secure_renegotiate, boolean()}
	| {send_timeout, timeout()}
	| {send_timeout_close, boolean()}
	| {verify, ssl:verify_type()}
	| {verify_fun, {fun(), InitialUserState::term()}}
	| {versions, [atom()]}].
-export_type([opts/0]).

name() -> ssl.

messages() -> {ssl, ssl_closed, ssl_error}.

-spec listen(opts()) -> {ok, ssl:sslsocket()} | {error, atom()}.
listen(Opts) ->
	ranch:require([crypto, asn1, public_key, ssl]),
	true = lists:keymember(cert, 1, Opts)
		orelse lists:keymember(certfile, 1, Opts),
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	Opts3 = ranch:set_option_default(Opts2, send_timeout, 30000),
	Opts4 = ranch:set_option_default(Opts3, send_timeout_close, true),
	Opts5 = ranch:set_option_default(Opts4, ciphers, unbroken_cipher_suites()),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	ssl:listen(0, ranch:filter_options(Opts5,
		[backlog, cacertfile, cacerts, cert, certfile, ciphers,
			fail_if_no_peer_cert, hibernate_after,
			honor_cipher_order, ip, key, keyfile, linger,
			next_protocols_advertised, nodelay,
			log_alert, password, port, raw,
			reuse_session, reuse_sessions, secure_renegotiate,
			send_timeout, send_timeout_close, verify, verify_fun,
			versions],
		[binary, {active, false}, {packet, raw},
			{reuseaddr, true}, {nodelay, true}])).

-spec accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	ssl:transport_accept(LSocket, Timeout).

-spec accept_ack(ssl:sslsocket(), timeout()) -> ok.
accept_ack(CSocket, Timeout) ->
	case ssl:ssl_accept(CSocket, Timeout) of
		ok ->
			ok;
		%% Garbage was most likely sent to the socket, don't error out.
		{error, {tls_alert, _}} ->
			ok = close(CSocket),
			exit(normal);
		%% Socket most likely stopped responding, don't error out.
		{error, timeout} ->
			ok = close(CSocket),
			exit(normal);
		{error, Reason} ->
			ok = close(CSocket),
			error(Reason)
	end.

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}]).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}],
		Timeout).

-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

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

%% @todo Probably filter Opts?
-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

-spec sockname(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	ssl:sockname(Socket).

-spec shutdown(ssl:sslsocket(), read | write | read_write)
	-> ok | {error, atom()}.
shutdown(Socket, How) ->
	ssl:shutdown(Socket, How).

-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

%% Internal.

%% Unfortunately the implementation of elliptic-curve ciphers that has
%% been introduced in R16B01 is incomplete.  Depending on the particular
%% client, this can cause the TLS handshake to break during key
%% agreement.  Depending on the ssl application version, this function
%% returns a list of all cipher suites that are supported by default,
%% minus the elliptic-curve ones.
-spec unbroken_cipher_suites() -> [ssl:erl_cipher_suite()].
unbroken_cipher_suites() ->
	case proplists:get_value(ssl_app, ssl:versions()) of
		Version when Version =:= "5.3"; Version =:= "5.3.1" ->
			lists:filter(fun(Suite) ->
				string:left(atom_to_list(element(1, Suite)), 4) =/= "ecdh"
			end, ssl:cipher_suites());
		_ ->
			ssl:cipher_suites()
	end.
