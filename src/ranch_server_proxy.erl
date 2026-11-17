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
			ok = claim_embedded(Ref),
			{ok, {Ref, Monitor}, hibernate};
		{error, Reason} ->
			%% init/1 does not call terminate/2.
			%% The stored owner may be a supervisor that already died.
			ok = release_owner(Ref),
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
	%% If this supervisor dies before ranch_server returns, the
	%% watcher releases the rows. It exits once ranch_server monitors
	%% the supervisor, so a later purge does not cancel that monitor.
	ranch_server:cleanup_listener_pids(Ref),
	[Sup | _] = get('$ancestors'),
	start_owner_watch(Ref, Sup);
terminate(_Reason, {Ref, _Monitor}) ->
	%% The table is owned by the ranch application process.
	%% A replacement supervisor may already own this ref.
	release_owner(Ref).

-spec code_change(term() | {down, term()},
	reference() | {ranch:ref(), reference()}, term())
	-> {ok, reference() | {ranch:ref(), reference()}}.
%% 2.4 kept only the monitor and did not trap exits.
code_change({down, _}, {Ref, Monitor}, _) ->
	process_flag(trap_exit, false),
	%% 2.4 does not release this key. Drop it without touching the
	%% option rows of a supervisor that is still running.
	ok = drop_owner_marker(Ref),
	{ok, Monitor};
code_change(_, Monitor, _) when is_reference(Monitor) ->
	process_flag(trap_exit, true),
	Ref = listener_ref(),
	ok = arm_embedded(Ref),
	{ok, {Ref, Monitor}};
code_change(_, State, _) ->
	{ok, State}.

listener_ref() ->
	[Sup | _] = get('$ancestors'),
	[{ranch_listener_sup, Ref}] = [Id ||
		{Id = {ranch_listener_sup, _}, _, _, _} <- supervisor:which_children(Sup)],
	Ref.

claim_embedded(Ref) ->
	[Sup | _] = get('$ancestors'),
	ranch_server:embedded_claim(Ref, Sup).

release_owner(Ref) ->
	[Sup | _] = get('$ancestors'),
	release_owner(Ref, Sup).

%% ranch_server may already be running the previous version.
release_owner(Ref, Sup) ->
	embedded_lock(Ref, fun() ->
		case ets:lookup(ranch_server, {embedded_sup, Ref}) of
			[{_, Owner}] when Owner =:= Sup ->
				ok = ranch_server:cleanup_listener_opts(Ref),
				ets:delete(ranch_server, {embedded_sup, Ref}),
				ok;
			[{_, Owner}] ->
				case is_process_alive(Owner) of
					true ->
						ok;
					false ->
						ok = ranch_server:cleanup_listener_opts(Ref),
						ets:delete(ranch_server, {embedded_sup, Ref}),
						ok
				end;
			[] ->
				ok
		end
	end).

drop_owner_marker(Ref) ->
	[Sup | _] = get('$ancestors'),
	embedded_lock(Ref, fun() ->
		case ets:lookup(ranch_server, {embedded_sup, Ref}) of
			[{_, Sup}] ->
				ets:delete(ranch_server, {embedded_sup, Ref}),
				ok;
			_ ->
				ok
		end
	end).

%% Same protocol as ranch_server's embedded lock.
embedded_lock(Ref, Fun) ->
	Lock = {embedded_lock, Ref},
	case ets:insert_new(ranch_server, {Lock, self()}) of
		true ->
			try Fun()
			after
				ets:delete(ranch_server, Lock)
			end;
		false ->
			case ets:lookup(ranch_server, Lock) of
				[{_, Holder}] ->
					case is_process_alive(Holder) of
						true ->
							timer:sleep(1);
						false ->
							ets:delete_object(ranch_server, {Lock, Holder})
					end;
				[] ->
					ok
			end,
			embedded_lock(Ref, Fun)
	end.

start_owner_watch(Ref, Sup) ->
	_ = spawn(fun() -> owner_watch(Ref, Sup) end),
	ok.

owner_watch(Ref, Sup) ->
	owner_watch(Ref, Sup, monitor(process, Sup)).

owner_watch(Ref, Sup, MRef) ->
	receive
		{'DOWN', MRef, process, Sup, _} ->
			release_owner(Ref, Sup)
	after 200 ->
		case server_monitors(Sup) orelse owner_moved(Ref, Sup) of
			true ->
				true = demonitor(MRef, [flush]),
				ok;
			false ->
				owner_watch(Ref, Sup, MRef)
		end
	end.

server_monitors(Sup) ->
	case whereis(ranch_server) of
		undefined ->
			false;
		Pid ->
			case process_info(Pid, monitors) of
				{monitors, Mons} ->
					lists:member({process, Sup}, Mons);
				_ ->
					false
			end
	end.

%% ranch_server is up and this supervisor no longer owns the ref.
owner_moved(Ref, Sup) ->
	case whereis(ranch_server) of
		undefined ->
			false;
		_ ->
			case ets:lookup(ranch_server, {embedded_sup, Ref}) of
				[{_, Sup}] ->
					false;
				_ ->
					true
			end
	end.

%% ranch_server may be suspended during an upgrade.
arm_embedded(Ref) ->
	[Sup | _] = get('$ancestors'),
	ok = ranch_server:note_embedded(Ref, Sup),
	case whereis(ranch_server) of
		undefined ->
			ok;
		Pid ->
			Pid ! {monitor_embedded, Ref, Sup},
			ok
	end.

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
