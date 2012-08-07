Ranch TCP Echo
==============

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then start telnet as indicated and type in a few lines. Be
aware that there is a timeout of 5 seconds without receiving
data before the example server disconnects your session.
