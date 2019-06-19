%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>
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

-module(havoc_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).
-import(ct_helper, [name/0]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

init_per_suite(Config) ->
	{ok, _} = application:ensure_all_started(ranch),
	ok = application:start(havoc),
	%% Enable logging of progress reports.
	%% They will only be available in the HTML reports by default.
	ok = logger:set_primary_config(level, info),
	Config.

end_per_suite(_) ->
	ok = application:stop(havoc),
	ok = application:stop(ranch).

%% Tests.

havoc_tcp(_) ->
	doc("Start a TCP listener, establish a hundred connections, "
		"run havoc, confirm we can still connect."),
	%% Start a TCP listener.
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		echo_protocol, []),
	Port1 = ranch:get_port(Name),
	%% Establish a hundred connections.
	_ = [begin
		{ok, Socket} = gen_tcp:connect("localhost", Port1, [{active, false}]),
		Socket
	end || _ <- lists:seq(1, 100)],
	%% Log process info of process about to be killed.
	LogFun = fun
		(Pid) when is_pid(Pid) ->
			logger:info("~p~n", [erlang:process_info(Pid)]);
		(Port) when is_port(Port) ->
			logger:info("~p~n", [erlang:port_info(Port)])
	end,
	%% Don't kill faster than ranch_sup can handle.
	KillInterval = 1000 * application:get_env(ranch, ranch_sup_period, 5),
	%% Run Havoc.
	havoc:on([{avg_wait, KillInterval}, {deviation, 0}, {applications, [ranch]},
		supervisor, {prekill_callback, LogFun}]),
	timer:sleep(10000),
	havoc:off(),
	timer:sleep(1000),
	%% Confirm we can still connect.
	Port2 = ranch:get_port(Name),
	{ok, _} = gen_tcp:connect("localhost", Port2, [{active, false}]),
	ok = ranch:stop_listener(Name).
