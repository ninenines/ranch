-module(embedded_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).
-export([stop/1]).
-export([start_listener/6]).
-export([stop_listener/2]).


start_link() ->
	supervisor:start_link(?MODULE, []).

stop(SupPid) ->
	erlang:exit(SupPid, normal).

init([]) ->
	ChildSpecs =
	case erlang:whereis(ranch_sup) of
		undefined ->
			[{ranch_sup, {ranch_sup, start_link, []}, permanent, 5000, supervisor, [ranch_sup]}];
		
		Pid when is_pid(Pid) ->
			%% ranch_sup already running
			[]
	end,
	
	{ok, {{one_for_one, 10, 10}, ChildSpecs}}.

start_listener(SupPid, Ref, Transport, TransOpts, Protocol, ProtoOpts) ->
	supervisor:start_child(
		SupPid,
		ranch:child_spec(Ref, Transport, TransOpts, Protocol, ProtoOpts)
	).

stop_listener(SupPid, Ref) ->
	ok = supervisor:terminate_child(SupPid, {ranch_listener_sup, Ref}),
	ok = supervisor:delete_child(SupPid, {ranch_listener_sup, Ref}),
	ranch_server:cleanup_listener_opts(Ref).

