%% Copyright (c) 2020-2021, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_erlang_transport).
-behaviour(ranch_transport).

-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
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

-type opts() :: [].
-export_type([opts/0]).

-spec name() -> erlang.
name() -> erlang.

-spec secure() -> boolean().
secure() ->
	false.

-spec messages() -> {erlang, erlang_closed, erlang_error, erlang_passive}.
messages() -> {erlang, erlang_closed, erlang_error, erlang_passive}.

-spec listen(ranch:transport_opts(opts())) -> {ok, reference()}.
listen(_TransOpts) ->
	{ok, make_ref()}.

-spec accept(reference(), timeout()) -> no_return(). % {ok, reference()}.
accept(_LSocket, _Timeout) ->
	receive after infinity -> {ok, make_ref()} end.

-spec handshake(reference(), timeout()) -> {ok, reference()}.
handshake(CSocket, Timeout) ->
	handshake(CSocket, [], Timeout).

-spec handshake(reference(), opts(), timeout()) -> {ok, reference()}.
handshake(CSocket, _, _) ->
	{ok, CSocket}.

-spec handshake_continue(reference(), timeout()) -> no_return().
handshake_continue(CSocket, Timeout) ->
	handshake_continue(CSocket, [], Timeout).

-spec handshake_continue(reference(), opts(), timeout()) -> no_return().
handshake_continue(_, _, _) ->
	error(not_supported).

-spec handshake_cancel(reference()) -> no_return().
handshake_cancel(_) ->
	error(not_supported).

-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, reference()}.
connect(_Host, Port, _Opts) when is_integer(Port) ->
	{ok, make_ref()}.

-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, reference()}.
connect(_Host, Port, _Opts, _Timeout) when is_integer(Port) ->
	{ok, make_ref()}.

-spec recv(reference(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(_Socket, _Length, _Timeout) ->
	{ok, <<>>}.

-spec recv_proxy_header(reference(), timeout()) -> no_return().
recv_proxy_header(_Socket, _Timeout) ->
	error(not_supported).

-spec send(reference(), iodata()) -> ok | {error, atom()}.
send(_Socket, _Packet) ->
	ok.

-spec sendfile(reference(), file:name_all() | file:fd())
	-> no_return().
sendfile(Socket, Filename) ->
	sendfile(Socket, Filename, 0, 0, []).

-spec sendfile(reference(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer())
	-> no_return().
sendfile(Socket, File, Offset, Bytes) ->
	sendfile(Socket, File, Offset, Bytes, []).

-spec sendfile(reference(), file:name_all() | file:fd(), non_neg_integer(),
		non_neg_integer(), [{chunk_size, non_neg_integer()}])
	-> no_return().
sendfile(_Socket, Filename, _Offset, _Bytes, _Opts)
		when is_list(Filename) orelse is_atom(Filename)
		orelse is_binary(Filename) ->
	error(not_supported).

-spec setopts(reference(), list()) -> ok.
setopts(_Socket, _Opts) ->
	ok.

-spec getopts(reference(), [atom()]) -> {ok, list()} | {error, atom()}.
getopts(_Socket, _Opts) ->
	{ok, []}.

-spec getstat(reference()) -> {ok, list()} | {error, atom()}.
getstat(_Socket) ->
	{ok, []}.

-spec getstat(reference(), [atom()]) -> {ok, list()} | {error, atom()}.
getstat(_Socket, _OptionNames) ->
	{ok, []}.

-spec controlling_process(reference(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(_Socket, _Pid) ->
	ok.

-spec peername(reference())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
peername(_Socket) ->
	{ok, {{127, 0, 0, 1}, 12701}}.

-spec sockname(reference())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
sockname(_Socket) ->
	{ok, {{127, 0, 0, 1}, 12710}}.

-spec shutdown(reference(), read | write | read_write)
	-> ok | {error, atom()}.
shutdown(_Socket, _How) ->
	ok.

-spec close(reference()) -> ok.
close(_Socket) ->
	ok.

-spec cleanup(ranch:transport_opts(opts())) -> ok.
cleanup(_) ->
	ok.
