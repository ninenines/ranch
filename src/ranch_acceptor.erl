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

%% @private
-module(ranch_acceptor).

%% API.
-export([start_link/6]).

%% Internal.
-export([acceptor/7]).

%% API.

-spec start_link(any(), inet:socket(), module(), module(), pid(), pid())
	-> {ok, pid()}.
start_link(Ref, LSocket, Transport, Protocol, ListenerPid, ConnsSup) ->
	{ok, Opts} = ranch_listener:get_protocol_options(ListenerPid),
	Pid = spawn_link(?MODULE, acceptor,
		[LSocket, Transport, Protocol, Opts, 1, ListenerPid, ConnsSup]),
	ok = ranch_server:add_acceptor(Ref, Pid),
	{ok, Pid}.

%% Internal.

-spec acceptor(inet:socket(), module(), module(), any(),
	non_neg_integer(), pid(), pid()) -> no_return().
acceptor(LSocket, Transport, Protocol, Opts, OptsVsn, ListenerPid, ConnsSup) ->
	Res = case Transport:accept(LSocket, 2000) of
		{ok, CSocket} ->
			{ok, Pid} = supervisor:start_child(ConnsSup,
				[ListenerPid, CSocket, Transport, Protocol, Opts]),
			Transport:controlling_process(CSocket, Pid),
			ranch_listener:add_connection(ListenerPid,
				default, Pid, OptsVsn);
		{error, timeout} ->
			ranch_listener:check_upgrades(ListenerPid, OptsVsn);
		{error, _Reason} ->
			%% @todo Probably do something here. If the socket was closed,
			%%       we may want to try and listen again on the port?
			ok
	end,
	case Res of
		ok ->
			?MODULE:acceptor(LSocket, Transport, Protocol,
				Opts, OptsVsn, ListenerPid, ConnsSup);
		{upgrade, Opts2, OptsVsn2} ->
			?MODULE:acceptor(LSocket, Transport, Protocol,
				Opts2, OptsVsn2, ListenerPid, ConnsSup)
	end.
