ranch_ssl
=========

The `ranch_ssl` module implements an SSL Ranch transport.

Types
-----

### opts() = [{backlog, non_neg_integer()}
	| {cacertfile, string()}
	| {cacerts, [Der::binary()]}
	| {cert, Der::binary()}
	| {certfile, string()}
	| {ciphers, [ssl:erl_cipher_suite()] | string()}
	| {fail_if_no_peer_cert, boolean()}
	| {hibernate_after, integer() | undefined}
	| {honor_cipher_order, boolean()}
	| {ip, inet:ip_address()}
	| {key, Der::binary()}
	| {keyfile, string()}
	| {linger, {boolean(), non_neg_integer()}}
	| {log_alert, boolean()}
	| {next_protocols_advertised, [binary()]}
	| {nodelay, boolean()}
	| {password, string()}
	| {port, inet:port_number()}
	| {raw, non_neg_integer(), non_neg_integer(), non_neg_integer() | binary()}
	| {reuse_session, fun()}
	| {reuse_sessions, boolean()}
	| {secure_renegotiate, boolean()}
	| {send_timeout, timeout()}
	| {send_timeout_close, boolean()}
	| {verify, ssl:verify_type()}
	| {verify_fun, {fun(), InitialUserState::term()}},
	| {versions, [atom()]}].

> Listen options.
>
> This does not represent the entirety of the options that can
> be set on the socket, but only the options that should be
> set independently of protocol implementation.

Option descriptions
-------------------

Specifying a certificate is mandatory, either through the `cert`
or the `certfile` option. None of the other options are required.

The default value is given next to the option name.

 -  backlog (1024)
   -  Max length of the queue of pending connections.
 -  cacertfile
   -  Path to PEM encoded trusted certificates file used to verify peer certificates.
 -  cacerts
   -  List of DER encoded trusted certificates.
 -  cert
   -  DER encoded user certificate.
 -  certfile
   -  Path to the PEM encoded user certificate file. May also contain the private key.
 -  ciphers
   -  List of ciphers that clients are allowed to use.
 -  fail_if_no_peer_cert (false)
   -  Whether to refuse the connection if the client sends an empty certificate.
 -  hibernate_after (undefined)
   -  Time in ms after which SSL socket processes go into hibernation to reduce memory usage.
 -  honor_cipher_order (false)
   -  If true, use the server's preference for cipher selection. If false (the default), use the client's preference.
 -  ip
   -  Interface to listen on. Listen on all interfaces by default.
 -  key
   -  DER encoded user private key.
 -  keyfile
   -  Path to the PEM encoded private key file, if different than the certfile.
 -  linger ({false, 0})
   -  Whether to wait and how long to flush data sent before closing the socket.
 -  log_alert (true)
   -  If false, error reports will not be displayed.
 -  next_protocols_advertised
   -  List of protocols to send to the client if it supports the Next Protocol extension.
 -  nodelay (true)
   -  Whether to enable TCP_NODELAY.
 -  password
   -  Password to the private key file, if password protected.
 -  port (0)
   -  TCP port number to listen on. 0 means a random port will be used.
 -  reuse_session
   -  Custom policy to decide whether a session should be reused.
 -  reuse_sessions (false)
   -  Whether to allow session reuse.
 -  secure_renegotiate (false)
   -  Whether to reject renegotiation attempts that do not conform to RFC5746.
 -  send_timeout (30000)
   -  How long the send call may wait for confirmation before returning.
 -  send_timeout_close (true)
   -  Whether to close the socket when the confirmation wasn't received.
 -  verify (verify_none)
   -  Use `verify_peer` to request a certificate from the client.
 -  verify_fun
   -  Custom policy to decide whether a client certificate is valid.
 -  versions
   -  TLS protocol versions that will be supported.

Note that the client will not send a certificate unless the
value for the `verify` option is set to `verify_peer`. This
means that the `fail_if_no_peer_cert` only apply when combined
with the `verify` option. The `verify_fun` option allows
greater control over the client certificate validation.

The `raw` option is unsupported.

Exports
-------

None.
