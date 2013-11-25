ranch
=====

The `ranch` module provides functions for starting and
manipulating Ranch listeners.

Types
-----

### max_conns() = non_neg_integer() | infinity

> Maximum number of connections allowed on this listener.
>
> This is a soft limit. The actual number of connections
> might be slightly above the limit due to concurrency
> when accepting new connections. Some connections may
> also be removed from this count explicitly by the user
> code.

### ref() = any()

> Unique name used to refer to a listener.

Exports
-------

### accept_ack(Ref) -> ok

> Types:
>  *  Ref = ref()
>
> Acknowledge that the connection is accepted.
>
> This function MUST be used by a connection process to inform
> Ranch that it initialized properly and let it perform any
> additional operations before the socket can be safely used.

### child_spec(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
	-> supervisor:child_spec()

> Types:
>  *  Ref = ref()
>  *  NbAcceptors = non_neg_integer()
>  *  Transport = module()
>  *  TransOpts = any()
>  *  Protocol = module()
>  *  ProtoOpts = any()
>
> Return child specifications for a new listener.
>
> This function can be used to embed a listener directly
> in an application instead of letting Ranch handle it.

### get_max_connections(Ref) -> MaxConns

> Types:
>  *  Ref = ref()
>  *  MaxConns = max_conns()
>
> Return the max number of connections allowed for the given listener.

### get_port(Ref) -> Port

> Types:
>  *  Ref = ref()
>  *  Port = inet:port_number()
>
> Return the port for the given listener.

### get_protocol_options(Ref) -> ProtoOpts

> Types:
>  *  Ref = ref()
>  *  ProtoOpts = any()
>
> Return the protocol options set for the given listener.

### remove_connection(Ref) -> ok

> Types:
>  *  Ref = ref()
>
> Do not count this connection when limiting the number of connections.
>
> You can use this function for long-running connection processes
> which spend most of their time idling rather than consuming
> resources. This allows Ranch to accept a lot more connections
> without sacrificing the latency of the system.
>
> This function may only be called from a connection process.

### set_max_connections(Ref, MaxConns) -> ok

> Types:
>  *  Ref = ref()
>  *  MaxConns = max_conns()
>
> Set the max number of connections for the given listener.
>
> The change will be applied immediately. If the new value is
> smaller than the previous one, Ranch will not kill the extra
> connections, but will wait for them to terminate properly.

### set_protocol_options(Ref, ProtoOpts) -> ok

> Types:
>  *  Ref = ref()
>  *  ProtoOpts = any()
>
> Set the protocol options for the given listener.
>
> The change will be applied immediately for all new connections.
> Old connections will not receive the new options.

### start_listener(Ref, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts)
	-> {ok, pid()} | {error, badarg}

> Types:
>  *  Ref = ref()
>  *  NbAcceptors = non_neg_integer()
>  *  Transport = module()
>  *  TransOpts = any()
>  *  Protocol = module()
>  *  ProtoOpts = any()
>
> Start listening for connections using the given transport
> and protocol. Returns the pid for this listener's supervisor.
>
> There are five additional transport options that apply
> regardless of transport. They allow configuring how the
> connections are supervised, rate limited and allow using
> an already open listening socket.
>
> The `ack_timeout` option defines how long post-accept socket
> initialization should take at a maximum. It defaults to `5000`.
>
> The `connection_type` option defines the type of process
> that will handle the connection. It can be either `worker`
> or `supervisor`. It defaults to `worker`.
>
> The `max_connections` option determines how many active
> connections are allowed before Ranch starts throttling
> the accept rate. This is a soft limit. It defaults to `1024`.
> Using the value `infinity` will disable this functionality
> entirely.
>
> The `shutdown` option determines the policy used with
> regards to connection processes when shutting down the listener.
> It can be either a positive integer indicating the max number
> of ms the supervisor will wait before forcibly killing the
> children, or the atom `brutal_kill`. It defaults to `5000`.
>
> The `socket` option allow passing an already open listening
> socket. In this case, Ranch will not call `Transport:listen/1`
> and so none of the transport specific options apply.

### stop_listener(Ref) -> ok | {error, not_found}

> Types:
>  *  Ref = ref()
>
> Stop the given listener.
>
> The listener is stopped gracefully, first by closing the
> listening port, then by stopping the connection processes.
> These processes are stopped according to the `shutdown`
> transport option, which may be set to brutally kill all
> connection processes or give them some time to stop properly.
>
> This function does not return until the listener is
> completely stopped.
