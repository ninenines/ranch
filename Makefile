# See LICENSE for licensing information.

PROJECT = ranch

# Dependencies.

TEST_DEPS = ct_helper
dep_ct_helper = https://github.com/extend/ct_helper.git master

# Options.

COMPILE_FIRST = ranch_transport
CT_SUITES = acceptor
PLT_APPS = crypto public_key ssl

# Standard targets.

include erlang.mk
