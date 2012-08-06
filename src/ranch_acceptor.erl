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
-export([init/7]).
-export([loop/7]).

%% API.

-spec start_link(any(), inet:socket(), module(), module(), pid(), pid())
	-> {ok, pid()}.
start_link(Ref, LSocket, Transport, Protocol, ListenerPid, ConnsSup) ->
	{ok, MaxConns} = ranch_listener:get_max_connections(ListenerPid),
	{ok, Opts} = ranch_listener:get_protocol_options(ListenerPid),
	Pid = spawn_link(?MODULE, init,
		[LSocket, Transport, Protocol, MaxConns, Opts, ListenerPid, ConnsSup]),
	ok = ranch_server:add_acceptor(Ref, Pid),
	{ok, Pid}.

%% Internal.

-spec init(inet:socket(), module(), module(),
	non_neg_integer(), any(), pid(), pid()) -> no_return().
init(LSocket, Transport, Protocol, MaxConns, Opts, ListenerPid, ConnsSup) ->
	async_accept(LSocket, Transport),
	loop(LSocket, Transport, Protocol, MaxConns, Opts, ListenerPid, ConnsSup).

-spec loop(inet:socket(), module(), module(),
	non_neg_integer(), any(), pid(), pid()) -> no_return().
loop(LSocket, Transport, Protocol, MaxConns, Opts, ListenerPid, ConnsSup) ->
	receive
		%% We couldn't accept the socket but it's safe to continue.
		{accept, continue} ->
			?MODULE:init(LSocket, Transport, Protocol,
				MaxConns, Opts, ListenerPid, ConnsSup);
		%% Found my sockets!
		{accept, CSocket} ->
			{ok, ConnPid} = supervisor:start_child(ConnsSup,
				[ListenerPid, CSocket, Transport, Protocol, Opts]),
			Transport:controlling_process(CSocket, ConnPid),
			ConnPid ! {shoot, ListenerPid},
			NbConns = ranch_listener:add_connection(ListenerPid, ConnPid),
			maybe_wait(ListenerPid, MaxConns, NbConns),
			?MODULE:init(LSocket, Transport, Protocol,
				MaxConns, Opts, ListenerPid, ConnsSup);
		%% Upgrade the protocol options.
		{set_opts, Opts2} ->
			?MODULE:loop(LSocket, Transport, Protocol,
				MaxConns, Opts2, ListenerPid, ConnsSup)
	end.

-spec maybe_wait(pid(), non_neg_integer(), non_neg_integer()) -> ok.
maybe_wait(_, MaxConns, NbConns) when MaxConns > NbConns ->
	ok;
maybe_wait(ListenerPid, MaxConns, _) ->
	erlang:yield(),
	NbConns2 = ranch_server:count_connections(ListenerPid),
	maybe_wait(ListenerPid, MaxConns, NbConns2).

-spec async_accept(inet:socket(), module()) -> ok.
async_accept(LSocket, Transport) ->
	AcceptorPid = self(),
	_ = spawn_link(fun() ->
		case Transport:accept(LSocket, infinity) of
			{ok, CSocket} ->
				Transport:controlling_process(CSocket, AcceptorPid),
				AcceptorPid ! {accept, CSocket};
			%% We want to crash if the listening socket got closed.
			{error, Reason} when Reason =/= closed ->
				AcceptorPid ! {accept, continue}
		end
	end),
	ok.
