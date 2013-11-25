ranch_protocol
==============

The `ranch_protocol` behaviour defines the interface used
by Ranch protocols.

Types
-----

None.

Callbacks
---------

### start_link(Ref, Socket, Transport, ProtoOpts) -> {ok, pid()}

> Types:
>  *  Ref = ranch:ref()
>  *  Socket = any()
>  *  Transport = module()
>  *  ProtoOpts = any()
>
> Start a new connection process for the given socket.
>
> The only purpose of this callback is to start a process that
> will handle the socket. It must spawn the process, link and
> then return the new pid. This function will always be called
> from inside a supervisor.
>
> If any other value is returned, the supervisor will close the
> socket and assume no process has been started.
>
> Do not perform any operation in this callback, as this would
> block the supervisor responsible for starting connection
> processes and degrade performance severely.
