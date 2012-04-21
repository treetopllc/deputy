.PHONY: all compile test clean

PROJECT = deputy
DIALYZER = dialyzer
REBAR = rebar

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	rm -f .eunit/*.dat
	@$(REBAR) skip_deps=true eunit

build_plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl

dialyzer:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns
docs:
	@$(REBAR) doc skip_deps=true
