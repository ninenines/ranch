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

%% @doc SSL transport API.
%%
%% Wrapper around <em>ssl</em> implementing the Ranch transport API.
%%
%% This transport requires the <em>crypto</em>, <em>asn1</em>,
%% <em>public_key</em> and <em>ssl</em> applications to be started.
%% If they aren't started, it will try to start them itself before
%% opening a port to listen. Applications aren't stopped when the
%% listening socket is closed, though.
%%
%% @see ssl
-module(ranch_ssl).
-behaviour(ranch_transport).

-export([name/0]).
-export([messages/0]).
-export([listen/1]).
-export([accept/2]).
-export([connect/3]).
-export([recv/3]).
-export([send/2]).
-export([sendfile/2]).
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([close/1]).

-type opts() :: [{backlog, non_neg_integer()}
	| {cacertfile, string()}
	| {cacerts, [Der::binary()]}
	| {cert, Der::binary()}
	| {certfile, string()}
	| {ciphers, [ssl:erl_cipher_suite()] | string()}
	| {fail_if_no_peer_cert, boolean()}
	| {ip, inet:ip_address()}
	| {key, Der::binary()}
	| {keyfile, string()}
	| {next_protocols_advertised, [binary()]}
	| {nodelay, boolean()}
	| {password, string()}
	| {port, inet:port_number()}
	| {raw, non_neg_integer(), non_neg_integer(),
		non_neg_integer() | binary()}
	| {reuse_session, fun()}
	| {reuse_sessions, boolean()}
	| {secure_renegotiate, boolean()}
	| {verify, ssl:verify_type()}
	| {verify_fun, {fun(), InitialUserState::term()}}].
-export_type([opts/0]).

%% @doc Name of this transport, <em>ssl</em>.
name() -> ssl.

%% @doc Atoms used to identify messages in {active, once | true} mode.
messages() -> {ssl, ssl_closed, ssl_error}.

%% @doc Listen for connections on the given port number.
%%
%% Calling this function returns a listening socket that can then
%% The available options are:
%%
%% <dl>
%%  <dt>backlog</dt><dd>Maximum length of the pending connections queue.
%%   Defaults to 1024.</dd>
%%  <dt>cacertfile</dt><dd>Optional. Path to file containing PEM encoded
%%   CA certificates (trusted certificates used for verifying a peer
%%   certificate).</dd>
%%  <dt>cert</dt><dd>Optional. The DER encoded users certificate. If this
%%   option is supplied it will override the certfile option.</dd>
%%  <dt>certfile</dt><dd>Mandatory. Path to a file containing the user's
%%   certificate.</dd>
%%  <dt>ciphers</dt><dd>Optional. The cipher suites that should be supported.
%%   The function ssl:cipher_suites/0 can be used to find all available
%%   ciphers.</dd>
%%  <dt>fail_if_no_peer_cert</dt><dd>Optional. Used together with {verify, verify_peer}.
%%   If set to true, the server will fail if the client does not have a certificate
%%   to send, i.e. sends a empty certificate, if set to false (that is by default)
%%   it will only fail if the client sends an invalid certificate (an empty
%%   certificate is considered valid).</dd>
%%  <dt>ip</dt><dd>Interface to listen on. Listen on all interfaces
%%   by default.</dd>
%%  <dt>key</dt><dd>Optional. The DER encoded users private key. If this option
%%   is supplied it will override the keyfile option.</dd>
%%  <dt>keyfile</dt><dd>Optional. Path to the file containing the user's
%%   private PEM encoded key.</dd>
%%  <dt>next_protocols_advertised</dt><dd>Optional. Erlang R16B+ required.
%%   List of protocols advertised by TLS Next Protocol Negotiation
%%   extension.</dd>
%%  <dt>nodelay</dt><dd>Optional. Enable TCP_NODELAY. Enabled by default.</dd>
%%  <dt>password</dt><dd>Optional. String containing the user's password.
%%   All private keyfiles must be password protected currently.</dd>
%%  <dt>port</dt><dd>TCP port number to open. Defaults to 0 (see below)</dd>
%%  <dt>reuse_session</dt><dd>Optional. Enables the ssl server to have a local
%%   policy for deciding if a session should be reused or not, only meaningful
%%   if reuse_sessions is set to true.</dd>
%%  <dt>reuse_sessions</dt><dd>Optional. Specifies if the server should agree
%%   to reuse sessions when the clients request to do so.</dd>
%%  <dt>secure_renegotiate</dt><dd>Optional. Specifies if to reject renegotiation
%%   attempt that does not live up to RFC 5746. By default secure_renegotiate is
%%   set to false i.e. secure renegotiation will be used if possible but it will
%%   fallback to unsecure renegotiation if the peer does not support RFC 5746.</dd>
%%  <dt>verify</dt><dd>Optional. If set to verify_peer, performs an x509-path
%%   validation and request the client for a certificate.</dd>
%%  <dt>verify_fun</dt><dd>Optional. The verify fun will be called during the
%%   X509-path validation when an error or an extension unknown to the ssl
%%   application is encountered. Additionally it will be called when a certificate
%%   is considered valid by the path validation to allow access to each certificate
%%   in the path to the user application.</dd>
%% </dl>
%%
%% You can listen to a random port by setting the port option to 0.
%% It is then possible to retrieve this port number by calling
%% sockname/1 on the listening socket. If you are using Ranch's
%% listener API, then this port number can obtained through
%% ranch:get_port/1 instead.
%%
%% @see ssl:listen/2
-spec listen(opts()) -> {ok, ssl:sslsocket()} | {error, atom()}.
listen(Opts) ->
	ranch:require([crypto, asn1, public_key, ssl]),
	true = lists:keymember(cert, 1, Opts)
		orelse lists:keymember(certfile, 1, Opts),
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	Opts3 = ranch:set_option_default(Opts2, ciphers, unbroken_cipher_suites()),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	ssl:listen(0, ranch:filter_options(Opts3,
		[backlog, cacertfile, cacerts, cert, certfile, ciphers,
			fail_if_no_peer_cert, ip, key, keyfile, next_protocols_advertised,
			nodelay, password, port, raw, reuse_session, reuse_sessions,
			secure_renegotiate, verify, verify_fun],
		[binary, {active, false}, {packet, raw},
			{reuseaddr, true}, {nodelay, true}])).

%% @doc Accept connections with the given listening socket.
%%
%% Note that this function does both the transport accept and
%% the SSL handshake. The returned socket is thus fully connected.
%%
%% @see ssl:transport_accept/2
%% @see ssl:ssl_accept/2
-spec accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom() | tuple()}.
accept(LSocket, Timeout) ->
	case ssl:transport_accept(LSocket, Timeout) of
		{ok, CSocket} ->
			ssl_accept(CSocket, Timeout);
		{error, Reason} ->
			{error, Reason}
	end.

%% @private Experimental. Open a connection to the given host and port number.
%% @see ssl:connect/3
%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}]).

%% @doc Receive data from a socket in passive mode.
%% @see ssl:recv/3
-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

%% @doc Send data on a socket.
%% @see ssl:send/2
-spec send(ssl:sslsocket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

%% @doc Send a file on a socket.
%%
%% Unlike with TCP, no syscall can be used here, so sending files
%% through SSL will be much slower in comparison.
%%
%% @see file:sendfile/2
-spec sendfile(ssl:sslsocket(), file:name())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filepath) ->
	{ok, IoDevice} = file:open(Filepath, [read, binary, raw]),
	sendfile(Socket, IoDevice, 0).

-spec sendfile(ssl:sslsocket(), file:io_device(), non_neg_integer())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, IoDevice, Sent) ->
	case file:read(IoDevice, 16#1FFF) of
		eof ->
			ok = file:close(IoDevice),
			{ok, Sent};
		{ok, Bin} ->
			case send(Socket, Bin) of
				ok ->
					sendfile(Socket, IoDevice, Sent + byte_size(Bin));
				{error, Reason} ->
					{error, Reason}
			end
	end.

%% @doc Set options on the given socket.
%% @see ssl:setopts/2
%% @todo Probably filter Opts?
-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

%% @doc Give control of the socket to a new process.
%%
%% Must be called from the process currently controlling the socket,
%% otherwise an {error, not_owner} tuple will be returned.
%%
%% @see ssl:controlling_process/2
-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

%% @doc Return the remote address and port of the connection.
%% @see ssl:peername/1
-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

%% @doc Return the local address and port of the connection.
%% @see ssl:sockname/1
-spec sockname(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	ssl:sockname(Socket).

%% @doc Close the given socket.
%% @see ssl:close/1
-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

%% Internal.

%% This call always times out, either because a numeric timeout value
%% was given, or because we've decided to use 5000ms instead of infinity.
%% This value should be reasonable enough for the moment.
-spec ssl_accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, {ssl_accept, atom()}}.
ssl_accept(Socket, infinity) ->
	ssl_accept(Socket, 5000);
ssl_accept(Socket, Timeout) ->
	case ssl:ssl_accept(Socket, Timeout) of
		ok ->
			{ok, Socket};
		{error, Reason} ->
			{error, {ssl_accept, Reason}}
	end.

%% Unfortunately the implementation of elliptic-curve ciphers that has
%% been introduced in R16B01 is incomplete.  Depending on the particular
%% client, this can cause the TLS handshake to break during key
%% agreement.  Depending on the ssl application version, this function
%% returns a list of all cipher suites that are supported by default,
%% minus the elliptic-curve ones.
-spec unbroken_cipher_suites() -> [ssl:erl_cipher_suite()].
unbroken_cipher_suites() ->
	case proplists:get_value(ssl_app, ssl:versions()) of
		"5.3" ->
			lists:filter(fun(Suite) ->
				string:left(atom_to_list(element(1, Suite)), 4) =/= "ecdh"
			end, ssl:cipher_suites());
		_ ->
			ssl:cipher_suites()
	end.
