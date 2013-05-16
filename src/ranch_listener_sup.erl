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
-export([start_link/6]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link(ranch:ref(), non_neg_integer(), module(), any(), module(), any())
	-> {ok, pid()}.
start_link(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts) ->
	MaxConns = proplists:get_value(max_connections, TransOpts, 1024),
	ranch_server:set_new_listener_opts(Ref, MaxConns, ProtoOpts),
	supervisor:start_link(?MODULE, {
		Ref, NbAcceptors, Transport, TransOpts, Protocol
	}).

%% supervisor.

init({Ref, NbAcceptors, Transport, TransOpts, Protocol}) ->
	ConnType = proplists:get_value(connection_type, TransOpts, worker),
	ChildSpecs = [
		%% conns_sup
		{ranch_conns_sup, {ranch_conns_sup, start_link,
				[Ref, ConnType, Transport, Protocol]},
			permanent, infinity, supervisor, [ranch_conns_sup]},
		%% acceptors_sup
		{ranch_acceptors_sup, {ranch_acceptors_sup, start_link,
				[Ref, NbAcceptors, Transport, TransOpts]
			}, permanent, infinity, supervisor, [ranch_acceptors_sup]}
	],
	{ok, {{rest_for_one, 10, 10}, ChildSpecs}}.
