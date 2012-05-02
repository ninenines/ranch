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
-export([start_link/5]).

%% Internal.
-export([init/6]).
-export([acceptor/6]).

%% API.

-spec start_link(inet:socket(), module(), module(), pid(), pid())
	-> {ok, pid()}.
start_link(LSocket, Transport, Protocol, ListenerPid, ConnsSup) ->
	{ok, Opts} = ranch_listener:get_protocol_options(ListenerPid),
	Pid = spawn_link(?MODULE, init,
		[LSocket, Transport, Protocol, Opts, ListenerPid, ConnsSup]),
	{ok, Pid}.

%% Internal.

-spec init(inet:socket(), module(), module(), any(), pid(), pid())
	-> no_return().
init(LSocket, Transport, Protocol, Opts, ListenerPid, ConnsSup) ->
	ranch_listener:acceptor_init_ack(ListenerPid),
	acceptor(LSocket, Transport, Protocol, Opts, ListenerPid, ConnsSup).

-spec acceptor(inet:socket(), module(), module(), any(), pid(), pid())
	-> no_return().
acceptor(LSocket, Transport, Protocol, Opts, ListenerPid, ConnsSup) ->
	async_accept(LSocket, Transport, self()),
	receive
		{accept, CSocket} ->
			{ok, ConnPid} = supervisor:start_child(ConnsSup,
				[ListenerPid, CSocket, Transport, Protocol, Opts]),
			Transport:controlling_process(CSocket, ConnPid),
			ranch_listener:add_connection(ListenerPid, default, ConnPid),
			ConnPid ! {shoot, ListenerPid},
			%% We want to suspend before accepting another connection.
			receive suspend ->
				receive resume -> flush() end
			after 0 ->
				ok
			end,
			?MODULE:acceptor(LSocket, Transport, Protocol,
				Opts, ListenerPid, ConnsSup);
		{upgrade, Opts2} ->
			?MODULE:acceptor(LSocket, Transport, Protocol,
				Opts2, ListenerPid, ConnsSup)
	end.

-spec async_accept(inet:socket(), module(), pid()) -> ok.
async_accept(LSocket, Transport, AcceptorPid) ->
	_ = spawn_link(fun() ->
		{ok, CSocket} = Transport:accept(LSocket, infinity),
		Transport:controlling_process(CSocket, AcceptorPid),
		AcceptorPid ! {accept, CSocket}
	end),
	ok.

-spec flush() -> ok.
flush() ->
	receive
		suspend -> flush();
		resume -> flush()
	after 0 ->
		ok
	end.
