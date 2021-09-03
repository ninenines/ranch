%% Copyright (c) 2015-2020, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_ct_hook).

-export([init/2]).

init(_, _) ->
	%% Allow a more relaxed restart intensity because
	%% some tests will cause quick restarts of several
	%% ranch_sup children.
	application:set_env(ranch, ranch_sup_intensity, 10),
	application:set_env(ranch, ranch_sup_period, 1),
	ok = application:load(ssl),
	case {os:type(), application:get_key(ssl, vsn)} of
		%% Internal active,N is broken on Windows since
		%% OTP 21.2/ssl 9.1.
		%% @todo Put an upper limit on the version when
		%% this is fixed in a future OTP version.
		{_, {ok, "9.0"++_}} ->
			ok;
		{{win32, nt}, {ok, "9."++_}} ->
			application:set_env(ssl, internal_active_n, 1);
		{{win32, nt}, {ok, "10."++_}} ->
			application:set_env(ssl, internal_active_n, 1);
		_ ->
			ok
	end,
	ct_helper:start([ranch]),
	ct_helper:make_certs_in_ets(),
	error_logger:add_report_handler(ct_helper_error_h),
	{ok, undefined}.
