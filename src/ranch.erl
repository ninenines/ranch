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

%% @doc Ranch API to start and stop listeners.
-module(ranch).

-export([start_listener/6]).
-export([stop_listener/1]).
-export([child_spec/6]).
-export([accept_ack/1]).
-export([get_port/1]).
-export([get_max_connections/1]).
-export([set_max_connections/2]).
-export([get_protocol_options/1]).
-export([set_protocol_options/2]).
-export([filter_options/3]).
-export([set_option_default/3]).
-export([require/1]).

%% @doc Start a listener for the given transport and protocol.
%%
%% A listener is effectively a pool of <em>NbAcceptors</em> acceptors.
%% Acceptors accept connections on the given <em>Transport</em> and forward
%% connections to the given <em>Protocol</em> handler. Both transport and
%% protocol modules can be given options through the <em>TransOpts</em> and
%% the <em>ProtoOpts</em> arguments. Available options are documented in the
%% <em>listen</em> transport function and in the protocol module of your choice.
%%
%% All acceptor and connection processes are supervised by the listener.
%%
%% It is recommended to set a large enough number of acceptors to improve
%% performance. The exact number depends of course on your hardware, on the
%% protocol used and on the number of expected simultaneous connections.
%%
%% The <em>Transport</em> option <em>max_connections</em> allows you to define
%% the maximum number of simultaneous connections for this listener. It defaults
%% to 1024. See <em>ranch_listener</em> for more details on limiting the number
%% of connections.
%%
%% <em>Ref</em> can be used to stop the listener later on.
%%
%% This function will return `{error, badarg}` if and only if the transport
%% module given doesn't appear to be correct.
-spec start_listener(any(), non_neg_integer(), module(), any(), module(), any())
	-> {ok, pid()} | {error, badarg}.
start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
		when is_integer(NbAcceptors) andalso is_atom(Transport)
		andalso is_atom(Protocol) ->
	_ = code:ensure_loaded(Transport),
	case erlang:function_exported(Transport, name, 0) of
		false ->
			{error, badarg};
		true ->
			Res = supervisor:start_child(ranch_sup, child_spec(Ref, NbAcceptors,
					Transport, TransOpts, Protocol, ProtoOpts)),
			case proplists:get_value(socket, TransOpts) of
				undefined ->
					ok;
				Socket ->
					%% change the controlling process so the caller dying doesn't
					%% close the port
					ListenerPid = ranch_server:lookup_listener(Ref),
					%%% Note: the catch is here because SSL crashes when you change
					%%% the controlling process of a listen socket because of a bug.
					%%% The bug will be fixed in R16.
					catch(Transport:controlling_process(Socket, ListenerPid))
			end,
			Res
	end.

%% @doc Stop a listener identified by <em>Ref</em>.
%%
%% Note that stopping the listener will close all currently running
%% connections abruptly.
-spec stop_listener(any()) -> ok | {error, not_found}.
stop_listener(Ref) ->
	case supervisor:terminate_child(ranch_sup, {ranch_listener_sup, Ref}) of
		ok ->
			supervisor:delete_child(ranch_sup, {ranch_listener_sup, Ref});
		{error, Reason} ->
			{error, Reason}
	end.

%% @doc Return a child spec suitable for embedding.
%%
%% When you want to embed Ranch in another application, you can use this
%% function to create a <em>ChildSpec</em> suitable for use in a supervisor.
%% The parameters are the same as in <em>start_listener/6</em> but rather
%% than hooking the listener to the Ranch internal supervisor, it just returns
%% the spec.
-spec child_spec(any(), non_neg_integer(), module(), any(), module(), any())
	-> supervisor:child_spec().
child_spec(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
		when is_integer(NbAcceptors) andalso is_atom(Transport)
		andalso is_atom(Protocol) ->
	{{ranch_listener_sup, Ref}, {ranch_listener_sup, start_link, [
		Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	]}, permanent, 5000, supervisor, [ranch_listener_sup]}.

%% @doc Acknowledge the accepted connection.
%%
%% Effectively used to make sure the socket control has been given to
%% the protocol process before starting to use it.
-spec accept_ack(pid()) -> ok.
accept_ack(ListenerPid) ->
	receive {shoot, ListenerPid} -> ok end.

%% @doc Return the listener's port.
-spec get_port(any()) -> inet:port_number().
get_port(Ref) ->
	ListenerPid = ranch_server:lookup_listener(Ref),
	{ok, Port} = ranch_listener:get_port(ListenerPid),
	Port.

%% @doc Return the max number of connections allowed concurrently.
-spec get_max_connections(any()) -> non_neg_integer().
get_max_connections(Ref) ->
	ListenerPid = ranch_server:lookup_listener(Ref),
	{ok, MaxConnections} = ranch_listener:get_max_connections(ListenerPid),
	MaxConnections.

%% @doc Set the max number of connections allowed concurrently.
-spec set_max_connections(any(), non_neg_integer()) -> ok.
set_max_connections(Ref, MaxConnections) ->
	ListenerPid = ranch_server:lookup_listener(Ref),
	ok = ranch_listener:set_max_connections(ListenerPid, MaxConnections).

%% @doc Return the current protocol options for the given listener.
-spec get_protocol_options(any()) -> any().
get_protocol_options(Ref) ->
	ListenerPid = ranch_server:lookup_listener(Ref),
	{ok, ProtoOpts} = ranch_listener:get_protocol_options(ListenerPid),
	ProtoOpts.

%% @doc Upgrade the protocol options for the given listener.
%%
%% The upgrade takes place at the acceptor level, meaning that only the
%% newly accepted connections receive the new protocol options. This has
%% no effect on the currently opened connections.
-spec set_protocol_options(any(), any()) -> ok.
set_protocol_options(Ref, ProtoOpts) ->
	ListenerPid = ranch_server:lookup_listener(Ref),
	ok = ranch_listener:set_protocol_options(ListenerPid, ProtoOpts).

%% @doc Filter a list of options and remove all unwanted values.
%%
%% It takes a list of options, a list of allowed keys and an accumulator.
%% This accumulator can be used to set default options that should never
%% be overriden.
-spec filter_options([{atom(), any()}], [atom()], Acc)
	-> Acc when Acc :: [any()].
filter_options([], _, Acc) ->
	Acc;
filter_options([Opt = {Key, _}|Tail], AllowedKeys, Acc) ->
	case lists:member(Key, AllowedKeys) of
		true -> filter_options(Tail, AllowedKeys, [Opt|Acc]);
		false -> filter_options(Tail, AllowedKeys, Acc)
	end.

%% @doc Add an option to a list, but only if it wasn't previously set.
-spec set_option_default(Opts, atom(), any())
	-> Opts when Opts :: [{atom(), any()}].
set_option_default(Opts, Key, Value) ->
	case lists:keymember(Key, 1, Opts) of
		true -> Opts;
		false -> [{Key, Value}|Opts]
	end.

%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
	ok;
require([App|Tail]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Tail).
