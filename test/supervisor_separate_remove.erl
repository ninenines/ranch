-module(supervisor_separate_remove).
-behavior(supervisor).
-behavior(ranch_protocol).

-export([start_link/3]).
-export([init/1]).

start_link(Ref, Transport, [{remove, MaybeRemove, Timeout}]) ->
	{ok, SupPid} = supervisor:start_link(?MODULE, []),
	{ok, ConnPid} = supervisor:start_child(SupPid,
		{remove_conn_and_wait_protocol,
			{remove_conn_and_wait_protocol, start_link,
				[Ref, Transport, [{remove, MaybeRemove, Timeout, SupPid}]]},
			temporary, 5000, worker, [remove_conn_and_wait_protocol]}),
	{ok, SupPid, ConnPid}.

init([]) ->
	{ok, {{one_for_one, 1, 1}, []}}.
