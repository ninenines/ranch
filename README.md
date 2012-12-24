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

Getting started
---------------

 *  [Read the guide](http://ninenines.eu/docs/en/ranch/HEAD/guide/introduction)
 *  Look at the examples in the `examples/` directory
 *  Build API documentation with `make docs`; open `doc/index.html`

Support
-------

 *  Official IRC Channel: #ninenines on irc.freenode.net
 *  [Mailing Lists](http://lists.ninenines.eu)
 *  [Commercial Support](http://ninenines.eu/support)
