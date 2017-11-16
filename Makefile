# See LICENSE for licensing information.

PROJECT = ranch
PROJECT_DESCRIPTION = Socket acceptor pool for TCP protocols.
PROJECT_VERSION = 1.4.0
PROJECT_REGISTERED = ranch_server

# Options.

CT_OPTS += -pa test -ct_hooks ranch_ct_hook []
PLT_APPS = crypto public_key ssl

# Dependencies.

LOCAL_DEPS = ssl

DOC_DEPS = asciideck

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

# CI configuration.

BUILD_DEPS = ci.erlang.mk
dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-18+
AUTO_CI_HIPE ?= OTP-LATEST
# AUTO_CI_ERLLVM ?= OTP-LATEST

# Standard targets.

include erlang.mk

# Dialyze the tests.

DIALYZER_OPTS += --src -r test

# Use erl_make_certs from the tested release during CI.

ci-setup:: $(DEPS_DIR)/ct_helper
	$(gen_verbose) cp ~/.kerl/builds/$(CI_OTP_RELEASE)/otp_src_git/lib/ssl/test/erl_make_certs.erl deps/ct_helper/src/
