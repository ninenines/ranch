%% Feel free to use, reuse and abuse the code in this file.

-module(reverse_protocol).
-behaviour(gen_statem).
-behaviour(ranch_protocol).

%% API.
-export([start_link/3]).

%% gen_statem.
-export([callback_mode/0]).
-export([init/1]).
-export([connected/3]).
-export([terminate/3]).
-export([code_change/4]).

-define(TIMEOUT, 60000).

-record(state, {ref, transport, socket}).

%% API.

start_link(Ref, Transport, Opts) ->
	gen_statem:start_link(?MODULE, {Ref, Transport, Opts}, []).

%% gen_statem.

callback_mode() ->
	[state_functions, state_enter].

init({Ref, Transport, _Opts = []}) ->
	{ok, connected, #state{ref=Ref, transport=Transport}, ?TIMEOUT}.

connected(enter, connected, StateData=#state{
		ref=Ref, transport=Transport}) ->
	{ok, Socket} = ranch:handshake(Ref),
	ok = Transport:setopts(Socket, [{active, once}, {packet, line}]),
	{keep_state, StateData#state{socket=Socket}};
connected(info, {tcp, Socket, Data}, _StateData=#state{
		socket=Socket, transport=Transport})
		when byte_size(Data) >= 1 ->
	Transport:setopts(Socket, [{active, once}]),
	Transport:send(Socket, reverse_binary(Data)),
	{keep_state_and_data, ?TIMEOUT};
connected(info, {tcp_closed, _Socket}, _StateData) ->
	{stop, normal};
connected(info, {tcp_error, _, Reason}, _StateData) ->
	{stop, Reason};
connected({call, From}, _Request, _StateData) ->
	gen_statem:reply(From, ok),
	keep_state_and_data;
connected(cast, _Msg, _StateData) ->
	keep_state_and_data;
connected(timeout, _Msg, _StateData) ->
	{stop, normal};
connected(_EventType, _Msg, _StateData) ->
	{stop, normal}.

terminate(Reason, StateName, StateData=#state{
		socket=Socket, transport=Transport})
		when Socket=/=undefined, Transport=/=undefined ->
	catch Transport:close(Socket),
	terminate(Reason, StateName,
		StateData#state{socket=undefined, transport=undefined});
terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% Internal.

reverse_binary(B0) when is_binary(B0) ->
	Size = bit_size(B0),
	<<B1:Size/integer-little>> = B0,
	case <<B1:Size/integer-big>> of
		%% Take care of different possible line terminators.
		<<$\n, $\r, B2/binary>> ->
			%% CR/LF (Windows)
			<<B2/binary, $\r, $\n>>;
		<<$\n, B2/binary>> ->
			%% LF (Linux, Mac OS X and later)
			<<B2/binary, $\n>>;
		<<$\r, B2/binary>> ->
			%% CR (Mac Classic, ie prior to Mac OS X)
			<<B2/binary, $\r>>
	end.
