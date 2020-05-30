REBAR := ./rebar3

all: $(REBAR)
	$(REBAR) escriptize

$(REBAR):
	curl -L https://s3.amazonaws.com/rebar3/rebar3 -o $@
	chmod +x $@

test:
	$(REBAR) do xref,eunit

clean:
	rm -rf _build $(REBAR)

.PHONY: all test clean
