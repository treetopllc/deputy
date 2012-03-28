.PHONY: all compile test clean

all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean

test:
	rm -f .eunit/*.dat
	@./rebar skip_deps=true eunit
