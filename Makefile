# See LICENSE for licensing information.

PROJECT = ranch

# Options.

COMPILE_FIRST = ranch_transport
CT_OPTS += -pa test -ct_hooks ranch_ct_hook []
PLT_APPS = crypto public_key ssl

CI_OTP_NO_SNI = \
	OTP_R16B OTP_R16B01 OTP_R16B02 OTP_R16B03-1 \
	OTP-17.1.2 OTP-17.2.2 OTP-17.3.4 OTP-17.4.1 OTP-17.5.6.6
CI_OTP ?= $(CI_OTP_NO_SNI) \
	OTP-18.0.3 OTP-18.1.5 OTP-18.2.4.1 OTP-18.3.4.4 \
	OTP-19.0.7 OTP-19.1.5

ifdef CI_OTP_RELEASE
ifneq ($(filter $(CI_OTP_RELEASE),$(CI_OTP_NO_SNI)),)
TEST_ERLC_OPTS += -DTEST_NO_SNI=1
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
