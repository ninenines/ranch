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
It receives four arguments: the listener's pid, the socket, the
transport handler being used and the protocol options defined in
the call to `ranch:start_listener/6`. This callback must
return `{ok, Pid}`, with `Pid` the pid of the new process.

The newly started process can then freely initialize itself. However,
it must call `ranch:accept_ack/1` before doing any socket operation.
This will ensure the connection process is the owner of the socket.
It expects the listener's pid as argument.

``` erlang
ok = ranch:accept_ack(ListenerPid).
```

If your protocol code requires specific socket options, you should
set them while initializing your connection process and before
starting `ranch:accept_ack/1`. You can use `Transport:setopts/2`
for that purpose.

Following is the complete protocol code for the example found
in `examples/tcp_echo/`.

``` erlang
-module(echo_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(ListenerPid),
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
