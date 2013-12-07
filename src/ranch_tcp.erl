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

%% @doc TCP transport API.
%%
%% Wrapper around <em>gen_tcp</em> implementing the Ranch transport API.
%%
%% @see gen_tcp
-module(ranch_tcp).
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
-export([close/1]).

-type opts() :: [{backlog, non_neg_integer()}
	| {ip, inet:ip_address()}
	| {nodelay, boolean()}
	| {port, inet:port_number()}
	| {raw, non_neg_integer(), non_neg_integer(),
		non_neg_integer() | binary()}].
-export_type([opts/0]).

%% @doc Name of this transport, <em>tcp</em>.
name() -> tcp.

%% @doc Atoms used to identify messages in {active, once | true} mode.
messages() -> {tcp, tcp_closed, tcp_error}.

%% @doc Listen for connections on the given port number.
%%
%% Calling this function returns a listening socket that can then
%% be passed to accept/2 to accept connections.
%%
%% The available options are:
%% <dl>
%%  <dt>backlog</dt><dd>Maximum length of the pending connections queue.
%%   Defaults to 1024.</dd>
%%  <dt>ip</dt><dd>Interface to listen on. Listen on all interfaces
%%   by default.</dd>
%%  <dt>nodelay</dt><dd>Optional. Enable TCP_NODELAY. Enabled by default.</dd>
%%  <dt>port</dt><dd>TCP port number to open. Defaults to 0 (see below).</dd>
%% </dl>
%%
%% You can listen to a random port by setting the port option to 0.
%% It is then possible to retrieve this port number by calling
%% sockname/1 on the listening socket. If you are using Ranch's
%% listener API, then this port number can obtained through
%% ranch:get_port/1 instead.
%%
%% @see gen_tcp:listen/2
-spec listen(opts()) -> {ok, inet:socket()} | {error, atom()}.
listen(Opts) ->
	Opts2 = ranch:set_option_default(Opts, backlog, 1024),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	gen_tcp:listen(0, ranch:filter_options(Opts2, [backlog, ip, nodelay, port, raw],
		[binary, {active, false}, {packet, raw},
			{reuseaddr, true}, {nodelay, true}])).

%% @doc Accept connections with the given listening socket.
%% @see gen_tcp:accept/2
-spec accept(inet:socket(), timeout())
	-> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	gen_tcp:accept(LSocket, Timeout).

-spec accept_ack(inet:socket(), timeout()) -> ok.
accept_ack(_, _) ->
	ok.

%% @private Experimental. Open a connection to the given host and port number.
%% @see gen_tcp:connect/3
%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	gen_tcp:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}]).

%% @private Experimental. Open a connection to the given host and port
%% number with a timeout.
%% @see gen_tcp:connect/4
%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	gen_tcp:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}],
		Timeout).

%% @doc Receive data from a socket in passive mode.
%% @see gen_tcp:recv/3
-spec recv(inet:socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout).

%% @doc Send data on a socket.
%% @see gen_tcp:send/2
-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	gen_tcp:send(Socket, Packet).

%% @equiv sendfile(Socket, File, Offset, Bytes, [])
-spec sendfile(inet:socket(), file:name_all() | file:fd())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) ->
	sendfile(Socket, Filename, 0, 0, []).

%% @equiv sendfile(Socket, File, Offset, Bytes, [])
-spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
	sendfile(Socket, File, Offset, Bytes, []).

%% @doc Send part of a file on a socket.
%%
%% As with sendfile/2 this is the optimal way to send (parts) of files using
%% TCP. Note that unlike file:sendfile/5 this function accepts either a raw file
%% or a file name and the ordering of arguments is different.
%%
%% @see file:sendfile/5
-spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer(), [{chunk_size, non_neg_integer()}])
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename, Offset, Bytes, Opts)
		when is_list(Filename) orelse is_atom(Filename)
		orelse is_binary(Filename) ->
	case file:open(Filename, [read, raw, binary]) of
		{ok, RawFile} ->
			try sendfile(Socket, RawFile, Offset, Bytes, Opts) of
				Result -> Result
			after
				ok = file:close(RawFile)
			end;
		{error, _} = Error ->
			Error
	end;
sendfile(Socket, RawFile, Offset, Bytes, Opts) ->
	Opts2 = case Opts of
		[] -> [{chunk_size, 16#1FFF}];
		_ -> Opts
	end,
	try file:sendfile(RawFile, Socket, Offset, Bytes, Opts2) of
		Result -> Result
	catch
		error:{badmatch, {error, enotconn}} ->
			%% file:sendfile/5 might fail by throwing a {badmatch, {error, enotconn}}
			%% this is because its internal implementation fails with a badmatch in
			%% prim_file:sendfile/10 if the socket is not connected.
			{error, closed}
	end.

%% @doc Set options on the given socket.
%% @see inet:setopts/2
%% @todo Probably filter Opts?
-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	inet:setopts(Socket, Opts).

%% @doc Give control of the socket to a new process.
%%
%% Must be called from the process currently controlling the socket,
%% otherwise an {error, not_owner} tuple will be returned.
%%
%% @see gen_tcp:controlling_process/2
-spec controlling_process(inet:socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	gen_tcp:controlling_process(Socket, Pid).

%% @doc Return the remote address and port of the connection.
%% @see inet:peername/1
-spec peername(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
peername(Socket) ->
	inet:peername(Socket).

%% @doc Return the local address and port of the connection.
%% @see inet:sockname/1
-spec sockname(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()}} | {error, atom()}.
sockname(Socket) ->
	inet:sockname(Socket).

%% @doc Close the given socket.
%% @see gen_tcp:close/1
-spec close(inet:socket()) -> ok.
close(Socket) ->
	gen_tcp:close(Socket).
