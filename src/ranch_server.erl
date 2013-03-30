%% Copyright (c) 2012, Loïc Hoguin <essen@ninenines.eu>
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
-module(ranch_server).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([insert_listener/2]).
-export([lookup_listener/1]).
-export([set_connections_sup/2]).
-export([lookup_connections_sup/1]).
-export([find_connections_sup/1]).
-export([count_connections/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAB, ?MODULE).

-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
	monitors = [] :: monitors()
}).

%% API.

%% @doc Start the ranch_server gen_server.
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Insert a listener into the database.
-spec insert_listener(any(), pid()) -> ok.
insert_listener(Ref, Pid) ->
	true = ets:insert_new(?TAB, {{listener, Ref}, Pid, undefined}),
	gen_server:cast(?MODULE, {insert_listener, Ref, Pid}).

%% @doc Lookup a listener in the database.
-spec lookup_listener(any()) -> pid().
lookup_listener(Ref) ->
	ets:lookup_element(?TAB, {listener, Ref}, 2).

%% @doc Set a connection supervisor associated with specific listener.
-spec set_connections_sup(any(), pid()) -> ok.
set_connections_sup(Ref, Pid) ->
	true = ets:update_element(?TAB, {listener, Ref}, {3, Pid}),
	true = ets:insert_new(?TAB, {{conns_sup, lookup_listener(Ref)}, Pid}),
	ok.

%% @doc Lookup a connection supervisor used by specific listener.
-spec lookup_connections_sup(any()) -> pid() | undefined.
lookup_connections_sup(Ref) ->
	ets:lookup_element(?TAB, {listener, Ref}, 3).

%% @doc Find a connection supervisor using the listener pid.
-spec find_connections_sup(pid()) -> pid().
find_connections_sup(Pid) ->
	ets:lookup_element(?TAB, {conns_sup, Pid}, 2).

%% @doc Count the number of connections in the connection pool.
-spec count_connections(any()) -> non_neg_integer().
count_connections(Ref) ->
	ranch_conns_sup:active_connections(lookup_connections_sup(Ref)).

%% gen_server.

%% @private
init([]) ->
	{ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast({insert_listener, Ref, Pid}, State=#state{monitors=Monitors}) ->
	MonitorRef = erlang:monitor(process, Pid),
	{noreply, State#state{
		monitors=[{{MonitorRef, Pid}, Ref}|Monitors]}};
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'DOWN', MonitorRef, process, Pid, _},
		State=#state{monitors=Monitors}) ->
	{_, Ref} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
	true = ets:delete(?TAB, {listener, Ref}),
	Monitors2 = lists:keydelete({MonitorRef, Pid}, 1, Monitors),
	{noreply, State#state{monitors=Monitors2}};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
