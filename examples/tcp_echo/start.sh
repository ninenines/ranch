#!/bin/sh
erl -pa ebin deps/*/ebin -s tcp_echo \
	-eval "io:format(\"Run: telnet localhost 5555~n\")."
