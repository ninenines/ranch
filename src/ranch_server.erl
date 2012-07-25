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
-export([add_acceptor/2]).
-export([send_to_acceptors/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAB, ?MODULE).

-type key() :: {listener | acceptors, any()}.
-type monitors() :: [{{reference(), pid()}, key()}].
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
	true = ets:insert_new(?TAB, {{listener, Ref}, Pid}),
	gen_server:cast(?MODULE, {insert_listener, Ref, Pid}).

%% @doc Lookup a listener in the database.
-spec lookup_listener(any()) -> pid().
lookup_listener(Ref) ->
	ets:lookup_element(?TAB, {listener, Ref}, 2).

%% @doc Add an acceptor for the given listener.
-spec add_acceptor(any(), pid()) -> ok.
add_acceptor(Ref, Pid) ->
	gen_server:cast(?MODULE, {add_acceptor, Ref, Pid}).

%% @doc Send a message to all acceptors of the given listener.
-spec send_to_acceptors(any(), any()) -> ok.
send_to_acceptors(Ref, Msg) ->
	Acceptors = ets:lookup_element(?TAB, {acceptors, Ref}, 2),
	_ = [Pid ! Msg || Pid <- Acceptors],
	ok.

%% gen_server.

%% @private
init([]) ->
	?TAB = ets:new(?TAB, [
		ordered_set, public, named_table, {read_concurrency, true}]),
	{ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast({insert_listener, Ref, Pid}, State=#state{monitors=Monitors}) ->
	true = ets:insert_new(?TAB, {{acceptors, Ref}, []}),
	MonitorRef = erlang:monitor(process, Pid),
	{noreply, State#state{
		monitors=[{{MonitorRef, Pid}, {listener, Ref}}|Monitors]}};
handle_cast({add_acceptor, Ref, Pid}, State=#state{monitors=Monitors}) ->
	MonitorRef = erlang:monitor(process, Pid),
	Acceptors = ets:lookup_element(?TAB, {acceptors, Ref}, 2),
	true = ets:insert(?TAB, {{acceptors, Ref}, [Pid|Acceptors]}),
	{noreply, State#state{
		monitors=[{{MonitorRef, Pid}, {acceptors, Ref}}|Monitors]}};
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'DOWN', MonitorRef, process, Pid, _},
		State=#state{monitors=Monitors}) ->
	{_, Key} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
	Monitors2 = remove_process(Key, MonitorRef, Pid, Monitors),
	{noreply, State#state{monitors=Monitors2}};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal.

-spec remove_process(key(), reference(), pid(), Monitors)
	-> Monitors when Monitors::monitors() .
remove_process(Key = {listener, _}, MonitorRef, Pid, Monitors) ->
	true = ets:delete(?TAB, Key),
	lists:keydelete({MonitorRef, Pid}, 1, Monitors);
remove_process(Key = {acceptors, _}, MonitorRef, Pid, Monitors) ->
	Acceptors = ets:lookup_element(?TAB, Key, 2),
	true = ets:insert(?TAB, {Key, lists:delete(Pid, Acceptors)}),
	lists:keydelete({MonitorRef, Pid}, 1, Monitors).
