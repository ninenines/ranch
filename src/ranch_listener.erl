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

%% @doc Public API for managing listeners.
-module(ranch_listener).
-behaviour(gen_server).

%% API.
-export([start_link/2]).
-export([stop/1]).
-export([add_connection/4]).
-export([move_connection/3]).
-export([remove_connection/2]).
-export([check_upgrades/2]).
-export([get_protocol_options/1]).
-export([set_protocol_options/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type pools() :: [{atom(), non_neg_integer()}].

-record(state, {
	conn_pools = [] :: pools(),
	conns_table :: ets:tid(),
	queue = undefined :: queue(),
	max_conns = undefined :: non_neg_integer(),
	proto_opts :: any(),
	proto_opts_vsn = 1 :: non_neg_integer()
}).

%% API.

%% @private
%%
%% We set the process priority to high because ranch_listener is the central
%% gen_server in Ranch and is used to manage all the incoming connections.
%% Setting the process priority to high ensures the connection-related code
%% will always be executed when a connection needs it, allowing Ranch to
%% scale far beyond what it would with a normal priority.
-spec start_link(non_neg_integer(), any()) -> {ok, pid()}.
start_link(MaxConns, ProtoOpts) ->
	gen_server:start_link(?MODULE, [MaxConns, ProtoOpts],
		[{spawn_opt, [{priority, high}]}]).

%% @private
-spec stop(pid()) -> stopped.
stop(ServerPid) ->
	gen_server:call(ServerPid, stop).

%% @doc Add a connection to the given pool in the listener.
%%
%% Pools of connections are used to restrict the maximum number of connections
%% depending on their type. By default, Ranch add all connections to the
%% pool <em>default</em>. It also checks for the maximum number of connections
%% in that pool before accepting again. This function only returns when there
%% is free space in the pool.
%%
%% When a process managing a connection dies, the process is removed from the
%% pool. If the socket has been sent to another process, it is up to the
%% protocol code to inform the listener of the new <em>ConnPid</em> by removing
%% the previous and adding the new one.
%%
%% This function also returns whether the protocol options have been modified.
%% If so, then an {upgrade, ProtoOpts, OptsVsn} will be returned instead of
%% the atom 'ok'. The acceptor can then continue with the new protocol options.
-spec add_connection(pid(), atom(), pid(), non_neg_integer())
	-> ok | {upgrade, any(), non_neg_integer()}.
add_connection(ServerPid, Pool, ConnPid, OptsVsn) ->
	gen_server:call(ServerPid, {add_connection, Pool, ConnPid, OptsVsn},
		infinity).

%% @doc Move a connection from one pool to another.
-spec move_connection(pid(), atom(), pid()) -> ok.
move_connection(ServerPid, DestPool, ConnPid) ->
	gen_server:cast(ServerPid, {move_connection, DestPool, ConnPid}).

%% @doc Remove the given connection from its pool.
-spec remove_connection(pid(), pid()) -> ok.
remove_connection(ServerPid, ConnPid) ->
	gen_server:cast(ServerPid, {remove_connection, ConnPid}).

%% @doc Return whether a protocol upgrade is required.
-spec check_upgrades(pid(), non_neg_integer())
	-> ok | {upgrade, any(), non_neg_integer()}.
check_upgrades(ServerPid, OptsVsn) ->
	gen_server:call(ServerPid, {check_upgrades, OptsVsn}).

%% @doc Return the current protocol options.
-spec get_protocol_options(pid()) -> {ok, any()}.
get_protocol_options(ServerPid) ->
	gen_server:call(ServerPid, get_protocol_options).

%% @doc Upgrade the protocol options.
-spec set_protocol_options(pid(), any()) -> ok.
set_protocol_options(ServerPid, ProtoOpts) ->
	gen_server:call(ServerPid, {set_protocol_options, ProtoOpts}).

%% gen_server.

%% @private
init([MaxConns, ProtoOpts]) ->
	ConnsTable = ets:new(connections_table, [set, private]),
	Queue = queue:new(),
	{ok, #state{conns_table=ConnsTable, max_conns=MaxConns,
		proto_opts=ProtoOpts, queue=Queue}}.

%% @private
handle_call({add_connection, Pool, ConnPid, AccOptsVsn}, From, State=#state{
		conn_pools=Pools, conns_table=ConnsTable,
		queue=Queue, max_conns=MaxConns,
		proto_opts=ProtoOpts, proto_opts_vsn=LisOptsVsn}) ->
	{NbConns, Pools2} = add_pid(ConnPid, Pool, Pools, ConnsTable),
	State2 = State#state{conn_pools=Pools2},
	if	AccOptsVsn =/= LisOptsVsn ->
			{reply, {upgrade, ProtoOpts, LisOptsVsn}, State2};
		NbConns > MaxConns ->
			Queue2 = queue:in(From, Queue),
			{noreply, State2#state{queue=Queue2}};
		true ->
			{reply, ok, State2}
	end;
handle_call({check_upgrades, AccOptsVsn}, _From, State=#state{
		proto_opts=ProtoOpts, proto_opts_vsn=LisOptsVsn}) ->
	if	AccOptsVsn =/= LisOptsVsn ->
			{reply, {upgrade, ProtoOpts, LisOptsVsn}, State};
		true ->
			{reply, ok, State}
	end;
handle_call(get_protocol_options, _From, State=#state{proto_opts=ProtoOpts}) ->
	{reply, {ok, ProtoOpts}, State};
handle_call({set_protocol_options, ProtoOpts}, _From,
		State=#state{proto_opts_vsn=OptsVsn}) ->
	{reply, ok, State#state{proto_opts=ProtoOpts, proto_opts_vsn=OptsVsn + 1}};
handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};
handle_call(_, _From, State) ->
	{reply, ignored, State}.

%% @private
handle_cast({move_connection, DestPool, ConnPid}, State=#state{
		conn_pools=Pools, conns_table=ConnsTable}) ->
	Pools2 = move_pid(ConnPid, DestPool, Pools, ConnsTable),
	{noreply, State#state{conn_pools=Pools2}};
handle_cast({remove_connection, ConnPid}, State=#state{
		conn_pools=Pools, conns_table=ConnsTable, queue=Queue}) ->
	{Pools2, Queue2} = remove_pid(ConnPid, Pools, ConnsTable, Queue),
	{noreply, State#state{conn_pools=Pools2, queue=Queue2}};
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info({'DOWN', _Ref, process, Pid, _Info}, State=#state{
		conn_pools=Pools, conns_table=ConnsTable, queue=Queue}) ->
	{Pools2, Queue2} = remove_pid(Pid, Pools, ConnsTable, Queue),
	{noreply, State#state{conn_pools=Pools2, queue=Queue2}};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

%% @private
-spec add_pid(pid(), atom(), pools(), ets:tid())
	-> {non_neg_integer(), pools()}.
add_pid(ConnPid, Pool, Pools, ConnsTable) ->
	MonitorRef = erlang:monitor(process, ConnPid),
	ConnPid ! {shoot, self()},
	{NbConnsRet, Pools2} = case lists:keyfind(Pool, 1, Pools) of
		false ->
			{1, [{Pool, 1}|Pools]};
		{Pool, NbConns} ->
			NbConns2 = NbConns + 1,
			{NbConns2, [{Pool, NbConns2}|lists:keydelete(Pool, 1, Pools)]}
	end,
	ets:insert(ConnsTable, {ConnPid, {MonitorRef, Pool}}),
	{NbConnsRet, Pools2}.

%% @private
-spec move_pid(pid(), atom(), pools(), ets:tid()) -> pools().
move_pid(ConnPid, DestPool, Pools, ConnsTable) ->
	{MonitorRef, SrcPool} = ets:lookup_element(ConnsTable, ConnPid, 2),
	ets:insert(ConnsTable, {ConnPid, {MonitorRef, DestPool}}),
	{SrcPool, SrcNbConns} = lists:keyfind(SrcPool, 1, Pools),
	DestNbConns = case lists:keyfind(DestPool, 1, Pools) of
		false -> 1;
		{DestPool, NbConns} -> NbConns + 1
	end,
	Pools2 = lists:keydelete(SrcPool, 1, lists:keydelete(DestPool, 1, Pools)),
	[{SrcPool, SrcNbConns - 1}, {DestPool, DestNbConns}|Pools2].

%% @private
-spec remove_pid(pid(), pools(), ets:tid(), queue()) -> {pools(), queue()}.
remove_pid(Pid, Pools, ConnsTable, Queue) ->
	{MonitorRef, Pool} = ets:lookup_element(ConnsTable, Pid, 2),
	erlang:demonitor(MonitorRef, [flush]),
	{Pool, NbConns} = lists:keyfind(Pool, 1, Pools),
	Pools2 = [{Pool, NbConns - 1}|lists:keydelete(Pool, 1, Pools)],
	ets:delete(ConnsTable, Pid),
	case queue:out(Queue) of
		{{value, Client}, Queue2} ->
			gen_server:reply(Client, ok),
			{Pools2, Queue2};
		_ ->
			{Pools2, Queue}
	end.
