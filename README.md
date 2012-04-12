Ranch
=====

Ranch is a socket acceptor pool for TCP protocols.

Goals
-----

Ranch aims to provide everything you need to accept TCP connections with
a **small** code base and **low latency** while being easy to use directly
as an application or to **embed** into your own.

Ranch provides a **modular** design, letting you choose which transport
and protocol are going to be used for a particular listener. Listeners
accept and manage connections on one port, and include facilities to
limit the number of **concurrent** connections. Connections are sorted
into **pools**, each pool having a different configurable limit.

Ranch also allows you to **upgrade** the acceptor pool without having
to close any of the currently opened sockets.

The project is currently in early development. Comments and suggestions are
more than welcome. To contribute, either open bug reports, or fork the project
and send us pull requests with new or improved functionality. You should
discuss your plans with us before doing any serious work, though, to avoid
duplicating efforts.

Quick start
-----------

* Add Ranch as a rebar or agner dependency to your application.
* Start Ranch and add one or more listeners.
* Write protocol handlers for your application.

Getting Started
---------------

Ranch accepts connections received on a given port and using a given
transport, like TCP or SSL, and forward them to a given protocol
handler. Acceptors and protocol handler processes are of course
supervised automatically.

Ranch does nothing by default. You need to explicitly request Ranch
to listen on a port with your chosen transport and protocol handlers.
To do so, you must start a listener.

A listener is a special kind of supervisor that manages both the
acceptor pool and the protocol processes. It is named and can thus be
started and stopped at will.

An acceptor pool is a pool of processes whose only role is to accept
new connections. It's good practice to have many of these processes
as they are very cheap and allow much quicker response when you get
many connections. Of course, as with everything else, you should
**benchmark** before you decide what's best for you.

Ranch includes both TCP and SSL transport handlers, abstracted through
a single common interface.

You can start and stop listeners by calling `ranch:start_listener/6` and
`ranch:stop_listener/1` respectively.

The following example demonstrates the startup of a very simple listener.

``` erlang
application:start(ranch),
%% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
ranch:start_listener(my_echo_listener, 100,
    ranch_tcp, [{port, 1234}],
    my_echo_protocol, [{log, "echo.log"}]
).
```

Writing a protocol handler
--------------------------

The only exported function a protocol handler needs is the start_link/4
function, with arguments ListenerPid, Socket, Transport and Opts. ListenerPid
is the pid to the listener's gen_server, managing the connections. Socket is of
course the client socket; Transport is the module name of the chosen transport
handler and Opts is protocol options defined when starting the listener.

After initializing your protocol, it is recommended to call the
function ranch:accept_ack/1 with the ListenerPid as argument,
as it will ensure Ranch has been able to fully initialize the socket.
Anything you do past this point is up to you!

If you need to change some socket options, like enabling raw mode for example,
you can call the <em>Transport:setopts/2</em> function. It is the protocol's
responsability to manage the socket usage, there should be no need for an user
to specify that kind of options while starting a listener.
