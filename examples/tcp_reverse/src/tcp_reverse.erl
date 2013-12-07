%% Feel free to use, reuse and abuse the code in this file.

-module(tcp_reverse).

%% API.
-export([start/0]).

%% API.

start() ->
    io:format("starting ranch and tcp_reverse~n"),
	ok = application:start(ranch),
	ok = application:start(tcp_reverse).
