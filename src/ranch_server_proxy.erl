%% Copyright (c) Jan Uhlig <juhlig@hnc-agency.org>
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

-module(ranch_server_proxy).

-behavior(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-spec start_link(ranch:ref()) -> {ok, pid()} | {error, term()}.
start_link(Ref) ->
	gen_server:start_link(?MODULE, Ref, []).

-spec init(ranch:ref()) -> {ok, {ranch:ref(), reference()}} | {stop, term()}.
init(Ref) ->
	process_flag(trap_exit, true),
	case wait_ranch_server(50) of
		{ok, Monitor} ->
			{ok, {Ref, Monitor}, hibernate};
		{error, Reason} ->
			%% init/1 does not call terminate/2.
			ranch_server:cleanup_listener_opts(Ref),
			{stop, Reason}
	end.

-spec handle_call(_, _, {ranch:ref(), reference()})
	-> {noreply, {ranch:ref(), reference()}, hibernate}.
handle_call(_, _, State) ->
	{noreply, State, hibernate}.

-spec handle_cast(_, {ranch:ref(), reference()})
	-> {noreply, {ranch:ref(), reference()}, hibernate}.
handle_cast(_, State) ->
	{noreply, State, hibernate}.

-spec handle_info(term(), {ranch:ref(), reference()})
	-> {noreply, {ranch:ref(), reference()}, hibernate}
		| {stop, term(), {ranch:ref(), reference()}}.
handle_info({'DOWN', Monitor, process, _, Reason}, {Ref, Monitor}) ->
	%% The parent stops this process with the atom shutdown.
	{stop, {shutdown, {ranch_server_down, Reason}}, {Ref, Monitor}};
handle_info(_, State) ->
	{noreply, State, hibernate}.

-spec terminate(term(), {ranch:ref(), reference()}) -> ok.
terminate({shutdown, {ranch_server_down, _}}, {Ref, _Monitor}) ->
	%% Option rows stay so a fast ranch_server restart can reattach.
	ranch_server:cleanup_listener_pids(Ref);
terminate(_Reason, {Ref, _Monitor}) ->
	%% The table is owned by the ranch application process.
	ranch_server:cleanup_listener_opts(Ref).

-spec code_change(term() | {down, term()},
	reference() | {ranch:ref(), reference()}, term())
	-> {ok, reference() | {ranch:ref(), reference()}}.
%% 2.4 kept only the monitor and did not trap exits.
code_change({down, _}, {_Ref, Monitor}, _) ->
	process_flag(trap_exit, false),
	{ok, Monitor};
code_change(_, Monitor, _) when is_reference(Monitor) ->
	process_flag(trap_exit, true),
	{ok, {listener_ref(), Monitor}};
code_change(_, State, _) ->
	{ok, State}.

listener_ref() ->
	[Sup | _] = get('$ancestors'),
	[{ranch_listener_sup, Ref}] = [Id ||
		{Id = {ranch_listener_sup, _}, _, _, _} <- supervisor:which_children(Sup)],
	Ref.

wait_ranch_server(N) ->
	case whereis(ranch_server) of
		undefined when N > 0 ->
			receive after 100 -> ok end,
			wait_ranch_server(N - 1);
		undefined ->
			{error, noproc};
		Pid ->
			Monitor = monitor(process, Pid),
			{ok, Monitor}
	end.
