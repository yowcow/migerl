REBAR_VERSION := 3.13.1
REBAR := ./rebar3

all: $(REBAR)
	$(REBAR) escriptize

$(REBAR):
	curl -L https://github.com/erlang/rebar3/releases/download/$(REBAR_VERSION)/rebar3 -o $@
	chmod 0755 $@

test:
	$(REBAR) eunit

clean:
	rm -rf _build $(REBAR)

.PHONY: all test clean
