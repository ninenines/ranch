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

-module(ranch_acceptors_sup).
-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-spec start_link(ranch:ref(), non_neg_integer(), module(), any())
	-> {ok, pid()}.
start_link(Ref, NbAcceptors, Transport, TransOpts) ->
	supervisor:start_link(?MODULE, [Ref, NbAcceptors, Transport, TransOpts]).

init([Ref, NbAcceptors, Transport, TransOpts]) ->
	ConnsSup = ranch_server:get_connections_sup(Ref),
	LSocket = case proplists:get_value(socket, TransOpts) of
		undefined ->
			{ok, Socket} = Transport:listen(TransOpts),
			Socket;
		Socket ->
			Socket
	end,
	{ok, {_, Port}} = Transport:sockname(LSocket),
	ranch_server:set_port(Ref, Port),
	Procs = [
		{{acceptor, self(), N}, {ranch_acceptor, start_link, [
			LSocket, Transport, ConnsSup
		]}, permanent, brutal_kill, worker, []}
			|| N <- lists:seq(1, NbAcceptors)],
	{ok, {{one_for_one, 10, 10}, Procs}}.
