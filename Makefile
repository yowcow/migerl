REBAR := rebar3

all:
	rebar3 escriptize

test:
	rebar3 eunit

clean:
	rm -rf _build

.PHONY: all test clean
