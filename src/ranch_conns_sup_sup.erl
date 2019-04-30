-module(ranch_conns_sup_sup).

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

start_link(Ref, NumAcceptors, Transport, Protocol) ->
	ok = ranch_server:cleanup_connections_sups(Ref),
	supervisor:start_link(?MODULE, {
		Ref, NumAcceptors, Transport, Protocol
	}).

init({Ref, NumAcceptors, Transport, Protocol}) ->
	ChildSpecs = [
		{{ranch_conns_sup, N}, {ranch_conns_sup, start_link,
				[Ref, N, Transport, Protocol]},
			permanent, infinity, supervisor, [ranch_conns_sup]}
		|| N <- lists:seq(1, NumAcceptors)],
	{ok, {{one_for_one, 1, 5}, ChildSpecs}}.
