%% Copyright (c) 2024, Jan Uhlig <juhlig@hnc-agency.org>
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

-module(ranch_listen_error_transport).
-behaviour(ranch_transport).

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
-export([shutdown/2]).
-export([close/1]).
-export([cleanup/1]).
-export([format_error/1]).

-type opt() :: ranch_tcp:opt().
-export_type([opt/0]).

-type opts() :: ranch_tcp:opts().
-export_type([opts/0]).

-spec name() -> tcp.
name() -> tcp.

-spec secure() -> boolean().
secure() ->
	false.

-spec messages() -> {tcp, tcp_closed, tcp_error, tcp_passive}.
messages() -> {tcp, tcp_closed, tcp_error, tcp_passive}.

-spec listen(ranch:transport_opts(opts())) -> {ok, inet:socket()} | {error, atom()}.
listen(_TransOpts) ->
	{error, {?MODULE, listen_error}}.

-spec disallowed_listen_options() -> [atom()].
disallowed_listen_options() ->
	ranch_tcp:disallowed_listen_options().

-spec accept(inet:socket(), timeout())
	-> {ok, inet:socket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	ranch_tcp:accept(LSocket, Timeout).

-spec handshake(inet:socket(), timeout()) -> {ok, inet:socket()}.
handshake(CSocket, Timeout) ->
	iranch_tcp:handshake(CSocket, Timeout).

-spec handshake(inet:socket(), opts(), timeout()) -> {ok, inet:socket()}.
handshake(CSocket, Opts, Timeout) ->
	ranch_tcp:handshake(CSocket, Opts, Timeout).

-spec handshake_continue(inet:socket(), timeout()) -> no_return().
handshake_continue(CSocket, Timeout) ->
	ranch_tcp:handshake_continue(CSocket, Timeout).

-spec handshake_continue(inet:socket(), opts(), timeout()) -> no_return().
handshake_continue(CSocket, Opts, Timeout) ->
	ranch_tcp:handshake_continue(CSocket, Opts, Timeout).

-spec handshake_cancel(inet:socket()) -> no_return().
handshake_cancel(CSocket) ->
	ranch_tcp:handshake_cancel(CSocket).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	ranch_tcp:connect(Host, Port, Opts).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	ranch_tcp:connect(Host, Port, Opts, Timeout).

-spec recv(inet:socket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ranch_tcp:recv(Socket, Length, Timeout).

-spec recv_proxy_header(inet:socket(), timeout())
	-> {ok, ranch_proxy_header:proxy_info()}
	| {error, closed | atom()}
	| {error, protocol_error, atom()}.
recv_proxy_header(Socket, Timeout) ->
	ranch_tcp:recv_proxy_header(Socket, Timeout).

-spec send(inet:socket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ranch_tcp:send(Socket, Packet).

-spec sendfile(inet:socket(), file:name_all() | file:fd())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) ->
	ranch_tcp:sendfile(Socket, Filename).

-spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
	ranch_tcp:sendfile(Socket, File, Offset, Bytes).

-spec sendfile(inet:socket(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer(), [{chunk_size, non_neg_integer()}])
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename, Offset, Bytes, Opts) ->
	ranch_tcp:sendfile(Socket, Filename, Offset, Bytes, Opts).

%% @todo Probably filter Opts?
-spec setopts(inet:socket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ranch_tcp:setopts(Socket, Opts).

-spec getopts(inet:socket(), [atom()]) -> {ok, list()} | {error, atom()}.
getopts(Socket, Opts) ->
	ranch_tcp:getopts(Socket, Opts).

-spec getstat(inet:socket()) -> {ok, list()} | {error, atom()}.
getstat(Socket) ->
	ranch_tcp:getstat(Socket).

-spec getstat(inet:socket(), [atom()]) -> {ok, list()} | {error, atom()}.
getstat(Socket, OptionNames) ->
	ranch_tcp:getstat(Socket, OptionNames).

-spec controlling_process(inet:socket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ranch_tcp:controlling_process(Socket, Pid).

-spec peername(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
peername(Socket) ->
	ranch_tcp:peername(Socket).

-spec sockname(inet:socket())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
sockname(Socket) ->
	ranch_tcp:sockname(Socket).

-spec shutdown(inet:socket(), read | write | read_write)
	-> ok | {error, atom()}.
shutdown(Socket, How) ->
	ranch_tcp:shutdown(Socket, How).

-spec close(inet:socket()) -> ok.
close(Socket) ->
	ranch_tcp:close(Socket).

-spec cleanup(ranch:transport_opts(opts())) -> ok.
cleanup(Opts) ->
	ranch_tcp:cleanup(Opts).

-spec format_error(inet:posix() | system_limit) -> string().
format_error({?MODULE, Reason}) ->
	io_lib:format("There was an error in ~0p: ~0p", [?MODULE, Reason]);
format_error(Reason) ->
	ranch_tcp:format_error(Reason).
