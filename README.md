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

* [Read the guide](https://github.com/extend/ranch/blob/master/guide/toc.md)
* Look at the examples in the ```examples/``` directory
* Build API documentation with ```make docs```; open ```doc/index.html```
