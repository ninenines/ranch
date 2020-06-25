-module(socket_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

%% ct.

all() ->
	[socket_test].

socket_test(_) ->
	_ = process_flag(trap_exit, true),
	{ok, S} = gen_tcp:listen(0, [{inet_backend, socket}, {packet, raw}, binary, {active, false}]),
%	{ok, {_, Port}} = inet:sockname(S),
%	spawn_link(
%		fun () ->
%			{ok, C} = gen_tcp:connect("localhost", Port, [{packet, raw}, binary, {active, false}]),
%			ok = gen_tcp:send(C, <<"foo">>),
%			{ok, <<"bar">>} = gen_tcp:recv(C, 0, 1000),
%			gen_tcp:close(C)
%		end
%	),
%	{ok, C} = gen_tcp:accept(S),
	{error, timeout} = gen_tcp:accept(S, 1000),
%	{ok, <<"foo">>} = gen_tcp:recv(C, 0, 1000),
%	ok = gen_tcp:send(C, <<"bar">>),
%	ok = gen_tcp:close(C),
%	ok = gen_tcp:close(S),
	ok.
