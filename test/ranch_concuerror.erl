%% Copyright (c) 2020-2021, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_concuerror).
-compile(export_all).
-compile(nowarn_export_all).

-concuerror_options([
	{after_timeout, 5000},
	{treat_as_normal, [
		killed, %% Acceptors are killed on shutdown.
		shutdown %% This is a normal exit reason in OTP.
	]}
]).

%% Convenience functions.

do_start() ->
	{ok, SupPid} = ranch_app:start(normal, []),
	SupPid.

-spec do_stop(pid()) -> no_return().
do_stop(SupPid) ->
	exit(SupPid, shutdown),
	%% We make sure that SupPid terminated before the test ends,
	%% because otherwise the shutdown will not be ordered and
	%% can produce error exit reasons.
	receive after infinity -> ok end.

%% Tests.

-spec start_stop() -> no_return().
start_stop() ->
	%% Start a listener then stop it.
	SupPid = do_start(),
	{ok, _} = ranch:start_listener(?FUNCTION_NAME,
		ranch_erlang_transport, #{
			num_acceptors => 1
		},
		echo_protocol, []),
	ok = ranch:stop_listener(?FUNCTION_NAME),
	do_stop(SupPid).

%% @todo This takes a huge amount of time.
%start_stop_twice() ->
%	%% Start a listener then stop it. Then start and stop it again.
%	SupPid = do_start(),
%	{ok, _} = ranch:start_listener(?FUNCTION_NAME,
%		ranch_erlang_transport, #{
%			num_acceptors => 1
%		},
%		echo_protocol, []),
%	ok = ranch:stop_listener(?FUNCTION_NAME),
%	{ok, _} = ranch:start_listener(?FUNCTION_NAME,
%		ranch_erlang_transport, #{
%			num_acceptors => 1
%		},
%		echo_protocol, []),
%	ok = ranch:stop_listener(?FUNCTION_NAME),
%	do_stop(SupPid).

-spec info() -> no_return().
info() ->
	%% Ensure we can call ranch:info/1 after starting a listener.
	SupPid = do_start(),
	{ok, _} = ranch:start_listener(?FUNCTION_NAME,
		ranch_erlang_transport, #{
			num_acceptors => 1
		},
		echo_protocol, []),
	#{} = ranch:info(?FUNCTION_NAME),
	do_stop(SupPid).
