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

``` bash
$ make
$ ./_rel/bin/tcp_echo console
```

You can then connect to it using telnet and see the echo server reply
everything you send to it. Then when you're done testing, you can use
the `Ctrl+]` key to escape to the telnet command line and type
`quit` to exit.

```
$ telnet localhost 5555
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

Default transport options
-------------------------

By default the socket will be set to return `binary` data, with the
options `{active, false}`, `{packet, raw}`, `{reuseaddr, true}` set.
These values can't be overriden when starting the listener, but
they can be overriden using `Transport:setopts/2` in the protocol.

It will also set `{backlog, 1024}` and `{nodelay, true}`, which
can be overriden at listener startup.

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

Listening on privileged ports
-----------------------------

Some systems limit access to ports below 1024 for security reasons.
This can easily be identified by an `{error, eacces}` error when trying
to open a listening socket on such a port.

The methods for listening on privileged ports vary between systems,
please refer to your system's documentation for more information.

We recommend the use of port rewriting for systems with a single server,
and load balancing for systems with multiple servers. Documenting these
solutions is however out of the scope of this guide.

Accepting connections on an existing socket
-------------------------------------------

If you want to accept connections on an existing socket, you can use the
`socket` transport option, which should just be the relevant data returned
from the connect function for the transport or the underlying socket library
(`gen_tcp:connect`, `ssl:connect`). The accept function will then be
called on the passed in socket. You should connect the socket in
`{active, false}` mode, as well.

Note, however, that because of a bug in SSL, you cannot change ownership of an
SSL listen socket prior to R16. Ranch will catch the error thrown, but the
owner of the SSL socket will remain as whatever process created the socket.
However, this will not affect accept behaviour unless the owner process dies,
in which case the socket is closed. Therefore, to use this feature with SSL
with an erlang release prior to R16, ensure that the SSL socket is opened in a
persistant process.

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
`ranch:remove_connection/1` from within the connection process,
with the name of the listener as the only argument.

``` erlang
ranch:remove_connection(Ref).
```

As seen in the chapter covering protocols, this pid is received as the
first argument of the protocol's `start_link/4` callback.

You can modify the `max_connections` value on a running listener by
using the `ranch:set_max_connections/2` function, with the name of the
listener as first argument and the new value as the second.

``` erlang
ranch:set_max_connections(tcp_echo, MaxConns).
```

The change will occur immediately.

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
