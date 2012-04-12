# See LICENSE for licensing information.

PROJECT = ranch

DIALYZER = dialyzer
REBAR = rebar

all: app

# Application.

deps:
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

docs: clean-docs
	@$(REBAR) doc skip_deps=true

clean-docs:
	rm -f doc/*.css
	rm -f doc/*.html
	rm -f doc/*.png
	rm -f doc/edoc-info

# Tests.

tests: clean app eunit ct

eunit:
	@$(REBAR) -C rebar.tests.config eunit skip_deps=true

ct:
	@$(REBAR) -C rebar.tests.config ct skip_deps=true

# Dialyzer.

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl tools inets crypto public_key ssl

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
