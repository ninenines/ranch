The Ranch Application
=====================

Socket acceptor pool for TCP protocols.

Dependencies
------------

The `ranch` application has no particular dependency required
to start.

It has optional dependencies that are only required when
listening for SSL connections. The dependencies are `crypto`,
`asn1`, `public_key` and `ssl`. They are started automatically
if they weren't before.

Environment
-----------

The `ranch` application defines one application environment
configuration parameter.

 -  profile (false)
   -  When enabled, Ranch will start `eprof` profiling automatically.

You can use the `ranch_app:profile_output/0` function to stop
profiling and output the results to the files `procs.profile`
and `total.profile`. Do not use in production.
