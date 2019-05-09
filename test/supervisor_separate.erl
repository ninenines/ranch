-module(supervisor_separate).
-behavior(supervisor).
-behavior(ranch_protocol).

-export([start_link/3]).
-export([init/1]).

start_link(Ref, Transport, Opts) ->
	{ok, SupPid} = supervisor:start_link(?MODULE, []),
	{ok, ConnPid} = supervisor:start_child(SupPid,
		{echo_protocol, {echo_protocol, start_link, [Ref, Transport, Opts]},
			temporary, 5000, worker, [echo_protocol]}),
	{ok, SupPid, ConnPid}.

init([]) ->
	{ok, {{one_for_one, 1, 1}, []}}.
