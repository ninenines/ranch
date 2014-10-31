%% Copyright (c) 2012-2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_server).
-behaviour(gen_server).

%% API.
%% 这个gen_server的作用相当与存放了很多全局的配置，管理很多的全局
%% 状态，变量，进程id
-export([start_link/0]).
-export([set_new_listener_opts/3]). %% 设置监听者参数
-export([cleanup_listener_opts/1]). %% 清除监听者参数
-export([set_connections_sup/2]). %% 设置conns_sup
-export([get_connections_sup/1]). %% 获取conns_sup
-export([set_port/2]). %% 设置端口
-export([get_port/1]). %% 获取端口
-export([set_max_connections/2]). %% 设置最大连接数 
-export([get_max_connections/1]). %% 获取最大连接数
-export([set_protocol_options/2]). %% 设置protocol参数
-export([get_protocol_options/1]). %% 获取protocol参数
-export([count_connections/1]). %% 计算当前连接数??? TODO

%% gen_server.
%% gen_server必须到处的API接口
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAB, ?MODULE). %% 表格的名字是ranch_server

-type monitors() :: [{{reference(), pid()}, any()}].
-record(state, {
	monitors = [] :: monitors()
}).

%% API.

%% 启动本模块
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec set_new_listener_opts(ranch:ref(), ranch:max_conns(), any()) -> ok.
%% API
set_new_listener_opts(Ref, MaxConns, Opts) ->
	gen_server:call(?MODULE, {set_new_listener_opts, Ref, MaxConns, Opts}).

-spec cleanup_listener_opts(ranch:ref()) -> ok.
%% 删除几个选项
cleanup_listener_opts(Ref) ->
	_ = ets:delete(?TAB, {port, Ref}), %% 端口
	_ = ets:delete(?TAB, {max_conns, Ref}), %% 最大连接数
	_ = ets:delete(?TAB, {opts, Ref}), %% 选项
	ok.

-spec set_connections_sup(ranch:ref(), pid()) -> ok.
%% API
set_connections_sup(Ref, Pid) ->
	true = gen_server:call(?MODULE, {set_connections_sup, Ref, Pid}),
	ok.

-spec get_connections_sup(ranch:ref()) -> pid().
%% 从ets表格中找到事先放在这里的conns_sup的pid
get_connections_sup(Ref) ->
	ets:lookup_element(?TAB, {conns_sup, Ref}, 2).

-spec set_port(ranch:ref(), inet:port_number()) -> ok.
set_port(Ref, Port) ->
	gen_server:call(?MODULE, {set_port, Ref, Port}).

-spec get_port(ranch:ref()) -> inet:port_number().
get_port(Ref) ->
	ets:lookup_element(?TAB, {port, Ref}, 2).

-spec set_max_connections(ranch:ref(), ranch:max_conns()) -> ok.
set_max_connections(Ref, MaxConnections) ->
	gen_server:call(?MODULE, {set_max_conns, Ref, MaxConnections}).

-spec get_max_connections(ranch:ref()) -> ranch:max_conns().
get_max_connections(Ref) ->
	ets:lookup_element(?TAB, {max_conns, Ref}, 2).

-spec set_protocol_options(ranch:ref(), any()) -> ok.
set_protocol_options(Ref, ProtoOpts) ->
	gen_server:call(?MODULE, {set_opts, Ref, ProtoOpts}).

-spec get_protocol_options(ranch:ref()) -> any().
get_protocol_options(Ref) ->
	ets:lookup_element(?TAB, {opts, Ref}, 2).

-spec count_connections(ranch:ref()) -> non_neg_integer().
count_connections(Ref) ->
	ranch_conns_sup:active_connections(get_connections_sup(Ref)).

%% gen_server.

init([]) ->
	%% 监听每个conns_sup的子进程??? TODO
	%% ets表格实在ranch_sup里面创建的
	Monitors = [{{erlang:monitor(process, Pid), Pid}, Ref} ||
		[Ref, Pid] <- ets:match(?TAB, {{conns_sup, '$1'}, '$2'})],
	{ok, #state{monitors=Monitors}}.

handle_call({set_new_listener_opts, Ref, MaxConns, Opts}, _, State) ->
	ets:insert(?TAB, {{max_conns, Ref}, MaxConns}),
	ets:insert(?TAB, {{opts, Ref}, Opts}),
	{reply, ok, State};
handle_call({set_connections_sup, Ref, Pid}, _,
		State=#state{monitors=Monitors}) ->
	%% 往ets表格中插入新的conn_sup
	case ets:insert_new(?TAB, {{conns_sup, Ref}, Pid}) of
		true ->
			%% 监控该conn_sup
			MonitorRef = erlang:monitor(process, Pid),
			{reply, true,
				State#state{monitors=[{{MonitorRef, Pid}, Ref}|Monitors]}};
		false ->
			%% 失败
			{reply, false, State}
	end;
handle_call({set_port, Ref, Port}, _, State) ->
	true = ets:insert(?TAB, {{port, Ref}, Port}),
	{reply, ok, State};
handle_call({set_max_conns, Ref, MaxConns}, _, State) ->
	ets:insert(?TAB, {{max_conns, Ref}, MaxConns}),
	ConnsSup = get_connections_sup(Ref),
	ConnsSup ! {set_max_conns, MaxConns},
	{reply, ok, State};
handle_call({set_opts, Ref, Opts}, _, State) ->
	ets:insert(?TAB, {{opts, Ref}, Opts}),
	ConnsSup = get_connections_sup(Ref),
	ConnsSup ! {set_opts, Opts},
	{reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _},
		State=#state{monitors=Monitors}) ->
	{_, Ref} = lists:keyfind({MonitorRef, Pid}, 1, Monitors),
	true = ets:delete(?TAB, {conns_sup, Ref}),
	Monitors2 = lists:keydelete({MonitorRef, Pid}, 1, Monitors),
	{noreply, State#state{monitors=Monitors2}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
