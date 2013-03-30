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
-export([start_link/3]).
-export([stop/1]).
-export([remove_connection/1]).
-export([get_port/1]).
-export([set_port/2]).
-export([get_max_connections/1]).
-export([set_max_connections/2]).
-export([get_protocol_options/1]).
-export([set_protocol_options/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	ref :: any(),
	max_conns = undefined :: ranch:max_conns(),
	port = undefined :: undefined | inet:port_number(),
	proto_opts = undefined :: any()
}).

%% API.

%% @private
-spec start_link(any(), non_neg_integer(), any()) -> {ok, pid()}.
start_link(Ref, MaxConns, ProtoOpts) ->
	gen_server:start_link(?MODULE, [Ref, MaxConns, ProtoOpts], []).

%% @private
-spec stop(pid()) -> stopped.
stop(ServerPid) ->
	gen_server:call(ServerPid, stop).

%% @doc Remove this process' connection from the pool.
%%
%% Useful if you have long-lived connections that aren't taking up
%% resources and shouldn't be counted in the limited number of running
%% connections.
-spec remove_connection(pid()) -> ok.
remove_connection(ServerPid) ->
	ConnsSup = ranch_server:find_connections_sup(ServerPid),
	ConnsSup ! {remove_connection, ServerPid},
	ok.

%% @doc Return the listener's port.
-spec get_port(pid()) -> {ok, inet:port_number()}.
get_port(ServerPid) ->
	gen_server:call(ServerPid, get_port).

%% @private
-spec set_port(pid(), inet:port_number()) -> ok.
set_port(ServerPid, Port) ->
	gen_server:cast(ServerPid, {set_port, Port}).

%% @doc Return the max number of connections allowed concurrently.
-spec get_max_connections(pid()) -> {ok, ranch:max_conns()}.
get_max_connections(ServerPid) ->
	gen_server:call(ServerPid, get_max_connections).

%% @doc Set the max number of connections allowed concurrently.
-spec set_max_connections(pid(), ranch:max_conns()) -> ok.
set_max_connections(ServerPid, MaxConnections) ->
	gen_server:call(ServerPid, {set_max_connections, MaxConnections}).

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
init([Ref, MaxConns, ProtoOpts]) ->
	ok = ranch_server:insert_listener(Ref, self()),
	{ok, #state{ref=Ref, max_conns=MaxConns, proto_opts=ProtoOpts}}.

%% @private
handle_call(get_port, _From, State=#state{port=Port}) ->
	{reply, {ok, Port}, State};
handle_call(get_max_connections, _From, State=#state{max_conns=MaxConns}) ->
	{reply, {ok, MaxConns}, State};
handle_call({set_max_connections, MaxConnections}, _From,
		State=#state{ref=Ref}) ->
	ConnsSup = ranch_server:lookup_connections_sup(Ref),
	ConnsSup ! {set_max_conns, MaxConnections},
	{reply, ok, State#state{max_conns=MaxConnections}};
handle_call(get_protocol_options, _From, State=#state{proto_opts=ProtoOpts}) ->
	{reply, {ok, ProtoOpts}, State};
handle_call({set_protocol_options, ProtoOpts}, _From, State=#state{ref=Ref}) ->
	ConnsSup = ranch_server:lookup_connections_sup(Ref),
	ConnsSup ! {set_opts, ProtoOpts},
	{reply, ok, State#state{proto_opts=ProtoOpts}};
handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};
handle_call(_, _From, State) ->
	{reply, ignored, State}.

%% @private
handle_cast({set_port, Port}, State) ->
	{noreply, State#state{port=Port}};
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @private
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
