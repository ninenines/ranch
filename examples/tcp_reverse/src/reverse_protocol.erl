%% Feel free to use, reuse and abuse the code in this file.

-module(reverse_protocol).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([start_link/4]).

-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([reverse_binary/1]).

-define(TIMEOUT, 5000).

-record(state, {socket, transport}).

reverse_binary(B) when is_binary(B) ->
   [list_to_binary(
       lists:reverse(
           binary_to_list(binary:part(B, {0, byte_size(B)-2})))),
    "\r\n"].

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Args) ->
    {ok, Args}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE,[], {state, Socket, Transport},?TIMEOUT).

handle_info({tcp, Socket, Data}, #state{transport = Transport} = State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    Transport:send(Socket, reverse_binary(Data)),
    {noreply, State, ?TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

