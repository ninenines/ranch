%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
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

-module(ranch).

-export([start_listener/6]).
-export([stop_listener/1]).
-export([child_spec/6]).
-export([accept_ack/1]).
-export([remove_connection/1]).
-export([get_port/1]).
-export([get_max_connections/1]).
-export([set_max_connections/2]).
-export([get_protocol_options/1]).
-export([set_protocol_options/2]).
-export([filter_options/3]).
-export([set_option_default/3]).
-export([require/1]).

-type max_conns() :: non_neg_integer() | infinity.
-export_type([max_conns/0]).

-type ref() :: any().
-export_type([ref/0]).

-spec start_listener(ref(), non_neg_integer(), module(), any(), module(), any())
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
			Socket = proplists:get_value(socket, TransOpts),
			case Res of
				{ok, Pid} when Socket =/= undefined ->
					%% Give ownership of the socket to ranch_acceptors_sup
					%% to make sure the socket stays open as long as the
					%% listener is alive. If the socket closes however there
					%% will be no way to recover because we don't know how
					%% to open it again.
					Children = supervisor:which_children(Pid),
					{_, AcceptorsSup, _, _}
						= lists:keyfind(ranch_acceptors_sup, 1, Children),
					%%% Note: the catch is here because SSL crashes when you change
					%%% the controlling process of a listen socket because of a bug.
					%%% The bug will be fixed in R16.
					catch Transport:controlling_process(Socket, AcceptorsSup);
				_ ->
					ok
			end,
			Res
	end.

-spec stop_listener(ref()) -> ok | {error, not_found}.
stop_listener(Ref) ->
	case supervisor:terminate_child(ranch_sup, {ranch_listener_sup, Ref}) of
		ok ->
			_ = supervisor:delete_child(ranch_sup, {ranch_listener_sup, Ref}),
			ranch_server:cleanup_listener_opts(Ref);
		{error, Reason} ->
			{error, Reason}
	end.

-spec child_spec(ref(), non_neg_integer(), module(), any(), module(), any())
	-> supervisor:child_spec().
child_spec(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
		when is_integer(NbAcceptors) andalso is_atom(Transport)
		andalso is_atom(Protocol) ->
	{{ranch_listener_sup, Ref}, {ranch_listener_sup, start_link, [
		Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
	]}, permanent, infinity, supervisor, [ranch_listener_sup]}.

-spec accept_ack(ref()) -> ok.
accept_ack(Ref) ->
	receive {shoot, Ref, Transport, Socket, AckTimeout} ->
		Transport:accept_ack(Socket, AckTimeout)
	end.

-spec remove_connection(ref()) -> ok.
remove_connection(Ref) ->
	ConnsSup = ranch_server:get_connections_sup(Ref),
	ConnsSup ! {remove_connection, Ref},
	ok.

-spec get_port(ref()) -> inet:port_number().
get_port(Ref) ->
	ranch_server:get_port(Ref).

-spec get_max_connections(ref()) -> max_conns().
get_max_connections(Ref) ->
	ranch_server:get_max_connections(Ref).

-spec set_max_connections(ref(), max_conns()) -> ok.
set_max_connections(Ref, MaxConnections) ->
	ranch_server:set_max_connections(Ref, MaxConnections).

-spec get_protocol_options(ref()) -> any().
get_protocol_options(Ref) ->
	ranch_server:get_protocol_options(Ref).

-spec set_protocol_options(ref(), any()) -> ok.
set_protocol_options(Ref, Opts) ->
	ranch_server:set_protocol_options(Ref, Opts).

-spec filter_options([{atom(), any()} | {raw, any(), any(), any()}],
	[atom()], Acc) -> Acc when Acc :: [any()].
filter_options(UserOptions, AllowedKeys, DefaultOptions) ->
	AllowedOptions = filter_user_options(UserOptions, AllowedKeys),
	lists:foldl(fun merge_options/2, DefaultOptions, AllowedOptions).

filter_user_options([Opt = {Key, _}|Tail], AllowedKeys) ->
	case lists:member(Key, AllowedKeys) of
		true -> [Opt|filter_user_options(Tail, AllowedKeys)];
		false -> filter_user_options(Tail, AllowedKeys)
	end;
filter_user_options([Opt = {raw, _, _, _}|Tail], AllowedKeys) ->
	case lists:member(raw, AllowedKeys) of
		true -> [Opt|filter_user_options(Tail, AllowedKeys)];
		false -> filter_user_options(Tail, AllowedKeys)
	end;
filter_user_options([], _) ->
	[].

merge_options({Key, _} = Option, OptionList) ->
	lists:keystore(Key, 1, OptionList, Option);
merge_options(Option, OptionList) ->
	[Option|OptionList].

-spec set_option_default(Opts, atom(), any())
	-> Opts when Opts :: [{atom(), any()}].
set_option_default(Opts, Key, Value) ->
	case lists:keymember(Key, 1, Opts) of
		true -> Opts;
		false -> [{Key, Value}|Opts]
	end.

-spec require([atom()]) -> ok.
require([]) ->
	ok;
require([App|Tail]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Tail).
