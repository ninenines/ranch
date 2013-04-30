# See LICENSE for licensing information.

PROJECT = ranch

# Options.

COMPILE_FIRST = ranch_transport
CT_SUITES = acceptor
PLT_APPS = crypto public_key ssl

# Standard targets.

include erlang.mk
