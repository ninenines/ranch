Ranch TCP Echo
==============

To build the example:
``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/tcp_echo_example console
```

Then start a telnet session to port 5555:
``` bash
$ telnet localhost 5555
```

Type in a few words and see them echoed back.

Be aware that there is a timeout of 5 seconds without receiving
data before the example server disconnects your session.
