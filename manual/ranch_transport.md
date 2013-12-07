ranch_transport
===============

The `ranch_transport` behaviour defines the interface used
by Ranch transports.

Types
-----

### sendfile_opts() = [{chunk_size, non_neg_integer()}]

> Options used by the sendfile function and callbacks.
>
> Allows configuring the chunk size, in bytes. Defaults to 8191 bytes.

Callbacks
---------

### accept(LSocket, Timeout)
	-> {ok, CSocket} | {error, closed | timeout | atom()}

> Types:
>  *  LSocket = CSocket = any()
>  *  Timeout = timeout()
>
> Accept a connection on the given listening socket.
>
> The `accept_ack` callback will be used to initialize the socket
> after accepting the connection. This is most useful when the
> transport is not raw TCP, like with SSL for example.

### accept_ack(CSocket, Timeout) -> ok

> Types:
>  *  CSocket = any()
>  *  Timeout = timeout()
>
> Perform post-accept initialization of the connection.
>
> This function will be called by connection processes
> before performing any socket operation. It allows
> transports that require extra initialization to perform
> their task and make the socket ready to use.

### close(CSocket) -> ok

> Types:
>  *  CSocket = any()
>
> Close the given socket.

### controlling_process(CSocket, Pid)
	-> ok | {error, closed | not_owner | atom()}

> Types:
>  *  CSocket = any()
>  *  Pid = pid()
>
> Change the controlling process for the given socket.
>
> The controlling process is the process that is allowed to
> perform operations on the socket, and that will receive
> messages from the socket when active mode is used. When
> the controlling process dies, the socket is closed.

### listen(TransOpts) -> {ok, LSocket} | {error, atom()}

> Types:
>  *  TransOpts = any()
>  *  LSocket = any()
>
> Listen for connections on the given port.
>
> The port is given as part of the transport options under
> the key `port`. Any other option is transport dependent.
>
> The socket returned by this call can then be used to
> accept connections. It is not possible to send or receive
> data from the listening socket.

### messages() -> {OK, Closed, Error}

> Types:
>  *  OK = Closed = Error = atom()
>
> Return the atoms used to identify messages sent in active mode.

### name() -> Name

> Types:
>  *  Name = atom()
>
> Return the name of the transport.

### peername(CSocket) -> {ok, {IP, Port}} | {error, atom()}

> Types:
>  *  CSocket = any()
>  *  IP = inet:ip_address()
>  *  Port = inet:port_number()
>
> Return the IP and port of the remote endpoint.

### recv(CSocket, Length, Timeout)
	-> {ok, Packet} | {error, closed | timeout | atom()}

> Types:
>  *  CSocket = any()
>  *  Length = non_neg_integer()
>  *  Timeout = timeout()
>  *  Packet = iodata() | any()
>
> Receive data from the given socket when in passive mode.
>
> Trying to receive data from a socket that is in active mode
> will return an error.
>
> A length of 0 will return any data available on the socket.
>
> While it is possible to use the timeout value `infinity`,
> this is highly discouraged as this could cause your process
> to get stuck waiting for data that will never come. This may
> happen when a socket becomes half-open due to a crash of the
> remote endpoint. Wi-Fi going down is another common culprit
> of this issue.

### send(CSocket, Packet) -> ok | {error, atom()}

> Types:
>  *  CSocket = any()
>  *  Packet = iodata()
>
> Send data to the given socket.

### sendfile(CSocket, File)
	-> sendfile(CSocket, File, 0, 0, [])
### sendfile(CSocket, File, Offset, Bytes)
	-> sendfile(CSocket, File, Offset, Bytes, [])
### sendfile(CSocket, File, Offset, Bytes, SfOpts)
	-> {ok, SentBytes} | {error, atom()}

> Types:
>  *  CSocket = any()
>  *  File = file:filename_all() | file:fd()
>  *  Offset = non_neg_integer()
>  *  Bytes = SentBytes = non_neg_integer()
>  *  SfOpts = sendfile_opts()
>
> Send data from a file to the given socket.
>
> The file may be sent full or in parts, and may be specified
> by its filename or by an already open file descriptor.
>
> Transports that manipulate TCP directly may use the
> `file:sendfile/{2,4,5}` function, which calls the sendfile
> syscall where applicable (on Linux, for example). Other
> transports can use the `sendfile/6` function exported from
> this module.

### setopts(CSocket, TransOpts) -> ok | {error, atom()}

> Types:
>  *  CSocket = any()
>  *  TransOpts = any()
>
> Change transport options for the given socket.
>
> This is mainly useful for switching to active or passive mode.

### shutdown(CSocket, How) -> ok | {error, atom()}

> Types:
>  *  CSocket = any()
>  *  How = read | write | read_write
>
> Immediately close the socket in one or two directions.

### sockname(CSocket) -> {ok, {IP, Port}} | {error, atom()}

> Types:
>  *  CSocket = any()
>  *  IP = inet:ip_address()
>  *  Port = inet:port_number()
>
> Return the IP and port of the local endpoint.

Exports
-------

### sendfile(Transport, CSocket, File, Offset, Bytes, SfOpts)
	-> {ok, SentBytes} | {error, atom()}

> Types:
>  *  Transport = module()
>  *  CSocket = any()
>  *  File = file:filename_all() | file:fd()
>  *  Offset = non_neg_integer()
>  *  Bytes = SentBytes = non_neg_integer()
>  *  SfOpts = sendfile_opts()
>
> Send data from a file to the given socket.
>
> This function emulates the function `file:sendfile/{2,4,5}`
> and may be used when transports are not manipulating TCP
> directly.
