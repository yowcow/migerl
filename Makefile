REBAR := rebar3

all:
	rebar3 escriptize

create-db: create-mysql create-postgres

create-mysql:
	RET=""; TRIES=30; \
	until [ "$$RET" != "" ] || [ $$TRIES -eq 0 ]; do \
		sleep 1; \
		RET=$$(echo "select 'ok'" | mysql -h127.0.0.1 -uroot); \
		TRIES=$$(($$TRIES - 1)); \
	done || true;
	echo "create database if not exists migerl_test default charset=utf8;" \
		| mysql -h127.0.0.1 -uroot

create-postgres:
	RET=""; TRIES=30; \
	until [ "$$RET" != "" ] || [ $$TRIES -eq 0 ]; do \
		sleep 1; \
		RET=$$(psql -c "select 1;" -U postgres -h 127.0.0.1 -p 5432); \
		TRIES=$$(($$TRIES - 1)); \
	done || true;
	psql -c "create database migerl_test;" -U postgres -h 127.0.0.1 -p 5432

test:
	rebar3 eunit

clean:
	rm -rf _build

.PHONY: all create-mysql test clean
