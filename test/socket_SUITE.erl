-module(socket_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

%% ct.

all() ->
	[socket_test].

socket_test(_) ->
	_ = process_flag(trap_exit, true),
	{ok, S} = gen_tcp:listen(0, [{inet_backend, socket}]),
	{ok, {_, Port}} = inet:sockname(S),
	spawn_link(
		fun () ->
			{ok, _} = gen_tcp:connect("localhost", Port, [])
		end
	),
	{ok, _} = gen_tcp:accept(S),
	ok.
