ranch_tcp
=========

The `ranch_tcp` module implements a TCP Ranch transport.

Note that due to bugs in OTP up to at least R16B02, it is
recommended to disable async threads when using the
`sendfile` function of this transport, as it can make
the threads stuck indefinitely.

Types
-----

### opts() = [{backlog, non_neg_integer()}
	| {ip, inet:ip_address()}
	| {linger, {boolean(), non_neg_integer()}}
	| {nodelay, boolean()}
	| {port, inet:port_number()}
	| {raw, non_neg_integer(), non_neg_integer(), non_neg_integer() | binary()}
	| {send_timeout, timeout()}
	| {send_timeout_close, boolean()}]

> Listen options.
>
> This does not represent the entirety of the options that can
> be set on the socket, but only the options that should be
> set independently of protocol implementation.

Option descriptions
-------------------

None of the options are required.

The default value is given next to the option name.

 -  backlog (1024)
   -  Max length of the queue of pending connections.
 -  ip
   -  Interface to listen on. Listen on all interfaces by default.
 -  linger ({false, 0})
   -  Whether to wait and how long to flush data sent before closing the socket.
 -  nodelay (true)
   -  Whether to enable TCP_NODELAY.
 -  port (0)
   -  TCP port number to listen on. 0 means a random port will be used.
 -  send_timeout (30000)
   -  How long the send call may wait for confirmation before returning.
 -  send_timeout_close (true)
   -  Whether to close the socket when the confirmation wasn't received.

The `raw` option is unsupported.

Exports
-------

None.
