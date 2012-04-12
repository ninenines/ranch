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

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]). %% ct.
-export([tcp_echo/1]). %% tcp.

%% ct.

all() ->
	[{group, tcp}].

groups() ->
	Tests = [tcp_echo],
	[{tcp, Tests}].

init_per_suite(Config) ->
	application:start(ranch),
	Config.

end_per_suite(_) ->
	application:stop(ranch),
	ok.

%% tcp.

tcp_echo(_) ->
	%% @todo Don't use a fixed port. start_listener should return the port used?
	{ok, _} = ranch:start_listener(tcp_echo, 1,
		ranch_tcp, [{port, 33333}],
		echo_protocol, []),
	{ok, Socket} = gen_tcp:connect("localhost", 33333,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, <<"Ranch is working!">>),
	{ok, <<"Ranch is working!">>} = gen_tcp:recv(Socket, 0, 1000),
	ok = ranch:stop_listener(tcp_echo),
	{error, closed} = gen_tcp:recv(Socket, 0, 1000),
	ok.
