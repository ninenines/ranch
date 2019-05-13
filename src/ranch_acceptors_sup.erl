%% Copyright (c) 2011-2018, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch_acceptors_sup).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

-spec start_link(ranch:ref(), pos_integer(), module())
	-> {ok, pid()}.
start_link(Ref, NumAcceptors, Transport) ->
	supervisor:start_link(?MODULE, [Ref, NumAcceptors, Transport]).

init([Ref, NumAcceptors, Transport]) ->
	TransOpts = ranch_server:get_transport_options(Ref),
	Logger = maps:get(logger, TransOpts, logger),
	NumListenSockets = maps:get(num_listen_sockets, TransOpts, 1),
	SocketOpts = maps:get(socket_opts, TransOpts, []),
	%% We temporarily put the logger in the process dictionary
	%% so that it can be used from ranch:filter_options. The
	%% interface as it currently is does not allow passing it
	%% down otherwise.
	put(logger, Logger),
	LSockets = start_listen_sockets(Ref, NumListenSockets, Transport, SocketOpts, Logger),
	erase(logger),
	Procs = [begin
		LSocketId = (AcceptorId rem NumListenSockets) + 1,
		{_, LSocket} = lists:keyfind(LSocketId, 1, LSockets),
		#{
			id => {acceptor, self(), AcceptorId},
			start => {ranch_acceptor, start_link, [Ref, AcceptorId, LSocket, Transport, Logger]},
			shutdown => brutal_kill
		}
	end || AcceptorId <- lists:seq(1, NumAcceptors)],
	{ok, {#{}, Procs}}.

-spec start_listen_sockets(any(), pos_integer(), module(), list(), module())
	-> [{pos_integer(), inet:socket()}].
start_listen_sockets(Ref, NumListenSockets, Transport, SocketOpts0, Logger) when NumListenSockets > 0 ->
	BaseSocket = start_listen_socket(Ref, Transport, SocketOpts0, Logger),
	{ok, Addr={_, Port}} = Transport:sockname(BaseSocket),
	SocketOpts = case lists:keyfind(port, 1, SocketOpts0) of
		{port, Port} ->
			SocketOpts0;
		_ ->
			[{port, Port}|lists:keydelete(port, 1, SocketOpts0)]
	end,
	ExtraSockets = [
		{N, start_listen_socket(Ref, Transport, SocketOpts, Logger)}
	|| N <- lists:seq(2, NumListenSockets)],
	ranch_server:set_addr(Ref, Addr),
	[{1, BaseSocket}|ExtraSockets].

-spec start_listen_socket(any(), module(), list(), module()) -> inet:socket().
start_listen_socket(Ref, Transport, SocketOpts, Logger) ->
	case Transport:listen(SocketOpts) of
		{ok, Socket} ->
			Socket;
		{error, Reason} ->
			listen_error(Ref, Transport, SocketOpts, Reason, Logger)
	end.

-spec listen_error(any(), module(), any(), atom(), module()) -> no_return().
listen_error(Ref, Transport, SocketOpts0, Reason, Logger) ->
	SocketOpts1 = [{cert, '...'}|proplists:delete(cert, SocketOpts0)],
	SocketOpts2 = [{key, '...'}|proplists:delete(key, SocketOpts1)],
	SocketOpts = [{cacerts, '...'}|proplists:delete(cacerts, SocketOpts2)],
	ranch:log(error,
		"Failed to start Ranch listener ~p in ~p:listen(~999999p) for reason ~p (~s)~n",
		[Ref, Transport, SocketOpts, Reason, format_error(Reason)], Logger),
	exit({listen_error, Ref, Reason}).

format_error(no_cert) ->
	"no certificate provided; see cert, certfile, sni_fun or sni_hosts options";
format_error(Reason) ->
	inet:format_error(Reason).
