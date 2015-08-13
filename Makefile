# See LICENSE for licensing information.

PROJECT = ranch

# Dependencies.

TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/extend/ct_helper.git master

# Options.

COMPILE_FIRST = ranch_transport
PLT_APPS = crypto public_key ssl
CI_OTP = \
	OTP_R15B01 OTP_R15B02 OTP_R15B03-1 \
	OTP_R16B OTP_R16B01 OTP_R16B02 OTP_R16B03-1 \
	OTP-17.0.2 OTP-17.1.2 OTP-17.2.2 OTP-17.3.4 OTP-17.4.1

# Standard targets.

include erlang.mk
