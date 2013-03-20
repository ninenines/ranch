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
%% This transport requires the <em>crypto</em>, <em>public_key</em>
%% and <em>ssl</em> applications to be started. If they aren't started,
%% it will try to start them itself before opening a port to listen.
%% Applications aren't stopped when the listening socket is closed, though.
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
-export([setopts/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([close/1]).

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
%%  <dt>certfile</dt><dd>Mandatory. Path to a file containing the user's
%%   certificate.</dd>
%%  <dt>ciphers</dt><dd>Optional. The cipher suites that should be supported.
%%   The function ssl:cipher_suites/0 can be used to find all available
%%   ciphers.</dd>
%%  <dt>ip</dt><dd>Interface to listen on. Listen on all interfaces
%%   by default.</dd>
%%  <dt>keyfile</dt><dd>Optional. Path to the file containing the user's
%%   private PEM encoded key.</dd>
%%  <dt>password</dt><dd>Optional. String containing the user's password.
%%   All private keyfiles must be password protected currently.</dd>
%%  <dt>port</dt><dd>TCP port number to open. Defaults to 0 (see below)</dd>
%% </dl>
%%
%% You can listen to a random port by setting the port option to 0.
%% It is then possible to retrieve this port number by calling
%% sockname/1 on the listening socket. If you are using Ranch's
%% listener API, then this port number can obtained through
%% ranch:get_port/1 instead.
%%
%% @see ssl:listen/2
-spec listen([{backlog, non_neg_integer()} | {cacertfile, string()}
	| {certfile, string()} | {ciphers, [ssl:erl_cipher_suite()] | string()}
	| {ip, inet:ip_address()} | {keyfile, string()} | {password, string()}
	| {port, inet:port_number()}])
	-> {ok, ssl:sslsocket()} | {error, atom()}.
listen(Opts) ->
	ranch:require([crypto, public_key, ssl]),
	{certfile, _} = lists:keyfind(certfile, 1, Opts),
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	ssl:listen(0, ranch:filter_options(Opts2,
		[backlog, cacertfile, certfile, ciphers, ip, keyfile, password, port],
		[binary, {active, false}, {packet, raw}, {reuseaddr, true}])).

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
-spec connect(string(), inet:port_number(), any()) -> {ok, ssl:sslsocket()} | {error, term()}.
connect(Host, Port, Opts) when is_list(Host), is_integer(Port) ->
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
-spec send(ssl:sslsocket(), iolist()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

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

-spec ssl_accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, {ssl_accept, atom()}}.
ssl_accept(Socket, Timeout) ->
	case ssl:ssl_accept(Socket, Timeout) of
		ok ->
			{ok, Socket};
		{error, Reason} ->
			{error, {ssl_accept, Reason}}
	end.
