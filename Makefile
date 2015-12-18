# See LICENSE for licensing information.

PROJECT = ranch

# Options.

COMPILE_FIRST = ranch_transport
CT_OPTS += -pa test -ct_hooks ranch_ct_hook []
PLT_APPS = crypto public_key ssl

CI_OTP = \
	OTP_R15B01 OTP_R15B02 OTP_R15B03-1 \
	OTP_R16B OTP_R16B01 OTP_R16B02 OTP_R16B03-1 \
	OTP-17.0.2 OTP-17.1.2 OTP-17.2.2 OTP-17.3.4 OTP-17.4.1 OTP-17.5.6.6 \
	OTP-18.0.3 OTP-18.1.5 OTP-18.2.1

# Only test on the most recent version on public CI services.
ifdef CI
ifndef BUILDKITE
CI_OTP := $(lastword $(CI_OTP))
endif
endif

# Dependencies.

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/ninenines/ct_helper master

# Standard targets.

include erlang.mk

# Also dialyze the tests.

DIALYZER_OPTS += --src -r test

# Use erl_make_certs from the tested release.

ci-setup:: $(DEPS_DIR)/ct_helper
	cp ~/.kerl/builds/$(CI_OTP_RELEASE)/otp_src_git/lib/ssl/test/erl_make_certs.erl deps/ct_helper/src/
