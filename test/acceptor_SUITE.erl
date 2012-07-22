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

-module(acceptor_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% ssl.
-export([ssl_echo/1]).

%% tcp.
-export([tcp_echo/1]).

%% ct.

all() ->
	[{group, tcp}, {group, ssl}].

groups() ->
	[{tcp, [
		tcp_echo
	]}, {ssl, [
		ssl_echo
	]}].

init_per_suite(Config) ->
	application:start(ranch),
	Config.

end_per_suite(_) ->
	application:stop(ranch),
	ok.

init_per_group(ssl, Config) ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	Config;
init_per_group(_, Config) ->
	Config.

end_per_group(ssl, _) ->
	application:stop(ssl),
	application:stop(public_key),
	application:stop(crypto),
	ok;
end_per_group(_, _) ->
	ok.

%% ssl.

ssl_echo(Config) ->
	{ok, _} = ranch:start_listener(ssl_echo, 1,
		ranch_ssl, [{port, 0},
			{certfile, ?config(data_dir, Config) ++ "cert.pem"}],
		echo_protocol, []),
	Port = ranch:get_port(ssl_echo),
	{ok, Socket} = ssl:connect("localhost", Port,
		[binary, {active, false}, {packet, raw},
		{certfile, ?config(data_dir, Config) ++ "cert.pem"}]),
	ok = ssl:send(Socket, <<"SSL Ranch is working!">>),
	{ok, <<"SSL Ranch is working!">>} = ssl:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(ssl_echo),
	{error, closed} = ssl:recv(Socket, 0, 1000),
	ok.

%% tcp.

tcp_echo(_) ->
	{ok, _} = ranch:start_listener(tcp_echo, 1,
		ranch_tcp, [{port, 0}], echo_protocol, []),
	Port = ranch:get_port(tcp_echo),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"TCP Ranch is working!">>),
	{ok, <<"TCP Ranch is working!">>} = gen_tcp:recv(Socket, 21, 1000),
	ok = ranch:stop_listener(tcp_echo),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	ok.
