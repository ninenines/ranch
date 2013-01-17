# See LICENSE for licensing information.

PROJECT = ranch
ERLC_OPTS = -Werror +debug_info +warn_export_all # +bin_opt_info +warn_missing_spec

DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

.PHONY: all clean-all app clean docs clean-docs tests build-plt dialyze

# Application.

all: app

clean-all: clean clean-docs
	rm -f .$(PROJECT).plt
	rm -rf $(DEPS_DIR) logs

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

app:
	@mkdir -p ebin/
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ \
		src/$(PROJECT)_transport.erl src/*.erl

clean:
	rm -rf ebin/
	rm -f test/*.beam
	rm -f erl_crash.dump

# Documentation.

docs: clean-docs
	erl -noshell -eval 'edoc:application($(PROJECT), ".", []), init:stop().'

clean-docs:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info

# Tests.

CT_RUN = ct_run \
	-pa ebin $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs \
	-cover test/cover.spec

tests: clean app
	@mkdir -p logs/
	@$(CT_RUN) -suite acceptor_SUITE

# Dialyzer.

build-plt: app
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib crypto public_key ssl

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
