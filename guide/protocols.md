Protocols
=========

Purpose
-------

A protocol handler starts a connection process and defines the
protocol logic executed in this process.

Writing a protocol handler
--------------------------

All protocol handlers must implement the `ranch_protocol` behavior
which defines a single callback, `start_link/4`. This callback is
responsible for spawning a new process for handling the connection.
It receives four arguments: the name of the listener, the socket, the
transport handler being used and the protocol options defined in
the call to `ranch:start_listener/6`. This callback must
return `{ok, Pid}`, with `Pid` the pid of the new process.

The newly started process can then freely initialize itself. However,
it must call `ranch:accept_ack/1` before doing any socket operation.
This will ensure the connection process is the owner of the socket.
It expects the listener's name as argument.

``` erlang
ok = ranch:accept_ack(Ref).
```

If your protocol code requires specific socket options, you should
set them while initializing your connection process, after
calling `ranch:accept_ack/1`. You can use `Transport:setopts/2`
for that purpose.

Following is the complete protocol code for the example found
in `examples/tcp_echo/`.

``` erlang
-module(echo_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            Transport:send(Socket, Data),
            loop(Socket, Transport);
        _ ->
            ok = Transport:close(Socket)
    end.
```

Using gen_server
----------------

Special processes like the ones that use the `gen_server` or `gen_fsm`
behaviours have the particularity of having their `start_link` call not
return until the `init` function returns. This is problematic, because
you won't be able to call `ranch:accept_ack/1` from the `init` callback
as this would cause a deadlock to happen.

There are two ways of solving this problem.

The first, and probably the most elegant one, is to make use of the
`gen_server:enter_loop/3` function. It allows you to start your process
normally (although it must be started with `proc_lib` like all special
processes), then perform any needed operations before falling back into
the normal `gen_server` execution loop.

``` erlang
-module(echo_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {ref, socket, transport, opts}).

-define(SERVER, ?MODULE).

start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).

init([Ref, Socket, Transport, ProtocolOpts]) ->
    ok = proc_lib:init_ack({ok, self()}),
    %% Perform any required state initialization here.
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{ref=Ref, socket=Socket, transport=Transport, opts= ProtocolOpts}).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data},
    State=#state{socket=Socket, transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, reverse_binary(Data)),
    {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    {stop, Reason, State}.
handle_info(_Info, State) ->
    {noreply, State}.

%% Other gen_server callbacks here.
```

The second method involves triggering a timeout just after `gen_server:init`
ends. If you return a timeout value of `0` then the `gen_server` will call
`handle_info(timeout, _, _)` right away.

``` erlang
-module(echo_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).


-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-record(state, {ref, socket, transport, opts}).

-define(SERVER, ?MODULE).

start_link(Ref, Socket, Transport, ProtocolOpts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, ProtocolOpts], []).

init([Ref, Socket, Transport, ProtocolOpts]) ->
    %% the 0 at the follwing line sets the timeout to 0, thus
    %% the timeout is called immediately after
    {ok,
    #state{ref=Ref, socket=Socket, transport=Transport, opts= ProtocolOpts}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Here the timeout handling
handle_info(timeout,
    State=#state{ref=Ref, socket=Socket, transport=Transport}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State};

handle_info({tcp, Socket, Data},
    State=#state{socket=Socket, transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, reverse_binary(Data)),
    {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    {stop, Reason, State}.
handle_info(_Info, State) ->
    {noreply, State}.
%% ...
```
