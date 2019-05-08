%% Copyright (c) 2019, Jan Uhlig <ju@mailingwork.de>
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

-module(ranch_conns_sup_sup).

-behaviour(supervisor).

-export([start_link/4]).
-export([init/1]).

-spec start_link(ranch:ref(), pos_integer(), ranch:opts(), module()) -> {ok, pid()}.
start_link(Ref, NumConnsSups, Transport, Protocol) ->
	ok = ranch_server:cleanup_connections_sups(Ref),
	supervisor:start_link(?MODULE, {
		Ref, NumConnsSups, Transport, Protocol
	}).

init({Ref, NumConnsSups, Transport, Protocol}) ->
	ChildSpecs = [
		{{ranch_conns_sup, N}, {ranch_conns_sup, start_link,
				[Ref, N, Transport, Protocol]},
			permanent, infinity, supervisor, [ranch_conns_sup]}
		|| N <- lists:seq(1, NumConnsSups)],
	{ok, {{one_for_one, 1, 5}, ChildSpecs}}.
