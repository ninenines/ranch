%% Copyright (c) 2013, Loïc Hoguin <essen@ninenines.eu>
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

-module(shutdown_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests.

-export([brutal_kill/1]).
-export([infinity/1]).
-export([infinity_trap_exit/1]).
-export([timeout/1]).
-export([timeout_trap_exit/1]).

%% ct.

all() ->
	[brutal_kill, infinity, infinity_trap_exit, timeout, timeout_trap_exit].

init_per_suite(Config) ->
	ok = application:start(ranch),
	Config.

end_per_suite(_) ->
	application:stop(ranch),
	ok.

%% Tests.

brutal_kill(_) ->
	Name = brutal_kill,
	{ok, ListenerSup} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {shutdown, brutal_kill}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _}
		= lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	ranch:stop_listener(Name),
	receive after 100 -> ok end,
	false = is_process_alive(Pid),
	false = is_process_alive(ListenerSup),
	{error, _} = gen_tcp:connect("localhost", Port, []),
	ok.

infinity(_) ->
	Name = infinity,
	{ok, ListenerSup} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {shutdown, infinity}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _}
		= lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	ranch:stop_listener(Name),
	receive after 100 -> ok end,
	false = is_process_alive(Pid),
	false = is_process_alive(ListenerSup),
	{error, _} = gen_tcp:connect("localhost", Port, []),
	ok.

infinity_trap_exit(_) ->
	Name = infinity_trap_exit,
	{ok, ListenerSup} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {shutdown, infinity}],
		trap_exit_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _}
		= lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	%% This call will block infinitely.
	SpawnPid = spawn(fun() -> ranch:stop_listener(Name) end),
	receive after 100 -> ok end,
	%% The protocol traps exit signals, and ignore them, so it won't die.
	true = is_process_alive(Pid),
	%% The listener will stay up forever too.
	true = is_process_alive(ListenerSup),
	%% We can't connect, though.
	{error, _} = gen_tcp:connect("localhost", Port, []),
	%% Killing the process unblocks everything.
	exit(Pid, kill),
	receive after 100 -> ok end,
	false = is_process_alive(ListenerSup),
	false = is_process_alive(SpawnPid),
	ok.

%% Same as infinity because the protocol doesn't trap exits.
timeout(_) ->
	Name = timeout,
	{ok, ListenerSup} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {shutdown, 500}],
		echo_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _}
		= lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	ranch:stop_listener(Name),
	receive after 100 -> ok end,
	false = is_process_alive(Pid),
	false = is_process_alive(ListenerSup),
	{error, _} = gen_tcp:connect("localhost", Port, []),
	ok.

timeout_trap_exit(_) ->
	Name = timeout_trap_exit,
	{ok, ListenerSup} = ranch:start_listener(Name, 1,
		ranch_tcp, [{port, 0}, {shutdown, 500}],
		trap_exit_protocol, []),
	Port = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port, []),
	receive after 100 -> ok end,
	ListenerSupChildren = supervisor:which_children(ListenerSup),
	{_, ConnsSup, _, _}
		= lists:keyfind(ranch_conns_sup, 1, ListenerSupChildren),
	[{_, Pid, _, _}] = supervisor:which_children(ConnsSup),
	true = is_process_alive(Pid),
	%% This call will block for the duration of the shutdown.
	SpawnPid = spawn(fun() -> ranch:stop_listener(Name) end),
	receive after 100 -> ok end,
	%% The protocol traps exit signals, and ignore them, so it won't die.
	true = is_process_alive(Pid),
	%% The listener will stay up for now too.
	true = is_process_alive(ListenerSup),
	%% We can't connect, though.
	{error, _} = gen_tcp:connect("localhost", Port, []),
	%% Wait for the timeout to finish and see that everything is killed.
	receive after 500 -> ok end,
	false = is_process_alive(Pid),
	false = is_process_alive(ListenerSup),
	false = is_process_alive(SpawnPid),
	ok.
