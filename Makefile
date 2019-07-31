REBAR := rebar3

all:
	rebar3 escriptize

create-db: create-mysql create-postgres

create-mysql:
	echo "create database if not exists migerl_test default charset=utf8;" \
		| mysql -h127.0.0.1 -uroot

create-postgres:
	psql -c "create database migerl_test;" -U postgres -h 127.0.0.1 -p 5432

test:
	rebar3 eunit

clean:
	rm -rf _build

.PHONY: all create-mysql test clean
