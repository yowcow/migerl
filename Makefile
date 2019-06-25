REBAR := rebar3

all:
	rebar3 escriptize

create-db: create-mysql

create-mysql:
	echo "create database if not exists migerl_test default charset=utf8;" \
		| mysql -h127.0.0.1 -uroot

test:
	rebar3 eunit

clean:
	rm -rf _build

.PHONY: all create-mysql test clean
