Listeners
=========

Purpose
-------

A listener is a set of processes whose role is to listen on a port
for new connections. It manages a pool of acceptor processes, each
of them indefinitely accepting connections. When it does, it starts
a new process executing the protocol handler code. All the socket
programming is abstracted through the user of transport handlers.

The listener takes care of supervising all the acceptor and connection
processes, allowing developers to focus on building their application.

Starting and stopping
---------------------

Ranch does nothing by default. It is up to the application developer
to request that Ranch listens for connections.

A listener can be started and stopped at will.

When starting a listener, a number of different settings are required:
 *  A name to identify it locally and be able to interact with it.
 *  The number of acceptors in the pool.
 *  A transport handler and its associated options.
 *  A protocol handler and its associated options.

Ranch includes both TCP and SSL transport handlers, respectively
`ranch_tcp` and `ranch_ssl`.

A listener can be started by calling the `ranch:start_listener/6`
function. Before doing so however, you must ensure that the `ranch`
application is started.

To start the `ranch` application:

``` erlang
ok = application:start(ranch).
```

You are then ready to start a listener. Let's call it `tcp_echo`. It will
have a pool of 100 acceptors, use a TCP transport and forward connections
to the `echo_protocol` handler.

``` erlang
{ok, _} = ranch:start_listener(tcp_echo, 100,
    ranch_tcp, [{port, 5555}],
    echo_protocol, []
).
```

You can try this out by compiling and running the `tcp_echo` example in the
examples directory. To do so, open a shell in the `examples/tcp_echo/`
directory and run the following commands:

```
% rebar get-deps compile
% ./start.sh
Listening on port 5555
```

You can then connect to it using telnet and see the echo server reply
everything you send to it. Then when you're done testing, you can use
the `Ctrl+]` key to escape to the telnet command line and type
`quit` to exit.

```
% telnet localhost 5555
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello!
Hello!
It works!
It works!
^]

telnet> quit
Connection closed.
```

Listening on a random port
--------------------------

You do not have to specify a specific port to listen on. If you give
the port number 0, or if you omit the port number entirely, Ranch will
start listening on a random port.

You can retrieve this port number by calling `ranch:get_port/1`. The
argument is the name of the listener you gave in `ranch:start_listener/6`.

``` erlang
{ok, _} = ranch:start_listener(tcp_echo, 100,
    ranch_tcp, [{port, 0}],
    echo_protocol, []
).
Port = ranch:get_port(tcp_echo).
```

Listening on a port =< 1024
---------------------------

This is currently not possible. We recommend the use of load balancing
or NAT firewall rules if the need arise. Proxies can sometimes also be
used although that's a less efficient solution.

Limiting the number of concurrent connections
---------------------------------------------

The `max_connections` transport option allows you to limit the number
of concurrent connections. It defaults to 1024. Its purpose is to
prevent your system from being overloaded and ensuring all the
connections are handled optimally.

``` erlang
{ok, _} = ranch:start_listener(tcp_echo, 100,
    ranch_tcp, [{port, 5555}, {max_connections, 100}],
    echo_protocol, []
).
```

You can disable this limit by setting its value to the atom `infinity`.

``` erlang
{ok, _} = ranch:start_listener(tcp_echo, 100,
    ranch_tcp, [{port, 5555}, {max_connections, infinity}],
    echo_protocol, []
).
```

You may not always want connections to be counted when checking for
`max_connections`. For example you might have a protocol where both
short-lived and long-lived connections are possible. If the long-lived
connections are mostly waiting for messages, then they don't consume
much resources and can safely be removed from the count.

To remove the connection from the count, you must call the
`ranch_listener:remove_connection/1` from within the connection process,
with the listener pid as the only argument.

``` erlang
ranch_listener:remove_connection(ListenerPid).
```

As seen in the chapter covering protocols, this pid is received as the
first argument of the protocol's `start_link/4` callback.

Upgrading
---------

Ranch allows you to upgrade the protocol options. This takes effect
immediately and for all subsequent connections.

To upgrade the protocol options, call `ranch:set_protocol_options/2`
with the name of the listener as first argument and the new options
as the second.

``` erlang
ranch:set_protocol_options(tcp_echo, NewOpts).
```

All future connections will use the new options.

You can also retrieve the current options similarly by
calling `ranch:get_protocol_options/1`.

``` erlang
Opts = ranch:get_protocol_options(tcp_echo).
```
