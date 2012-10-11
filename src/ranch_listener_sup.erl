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

%% @private
-module(ranch_listener_sup).
-behaviour(supervisor).

%% API.
-export([start_link/7]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link(any(), non_neg_integer(), module(), any(), module(), any(), any())
	-> {ok, pid()}.
start_link(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts, AccOpts) ->
	MaxConns = proplists:get_value(max_connections, TransOpts, 1024),
	{ok, SupPid} = supervisor:start_link(?MODULE, []),
	{ok, ListenerPid} = supervisor:start_child(SupPid,
		{ranch_listener, {ranch_listener, start_link,
			[Ref, MaxConns, ProtoOpts]},
		 permanent, 5000, worker, [ranch_listener]}),
	ok = ranch_server:insert_listener(Ref, ListenerPid),
	{ok, ConnsPid} = supervisor:start_child(SupPid,
		{ranch_conns_sup, {ranch_conns_sup, start_link, []},
		 permanent, 5000, supervisor, [ranch_conns_sup]}),
	{ok, _PoolPid} = supervisor:start_child(SupPid,
		{ranch_acceptors_sup, {ranch_acceptors_sup, start_link, [
			Ref, NbAcceptors, Transport, TransOpts,
			Protocol, ListenerPid, ConnsPid, AccOpts
		]}, permanent, 5000, supervisor, [ranch_acceptors_sup]}),
	{ok, SupPid}.

%% supervisor.

init([]) ->
	{ok, {{one_for_all, 10, 10}, []}}.
