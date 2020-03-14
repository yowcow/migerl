MYSQL := mysql:5.7
POSTGRES := postgres:11

PASSWORD ?= "Passw0rd"

all: pull

pull:
	$(MAKE) -f docker.mk -j 2 pull-mysql pull-postgres

pull-mysql:
	docker pull $(MYSQL)

pull-postgres:
	docker pull $(POSTGRES)

start:
	$(MAKE) -f docker.mk -j 2 mysql.cid postgres.cid

mysql.cid:
	docker run --rm -d \
		--cidfile $@ \
		--name migerl-mysql \
		-e MYSQL_ALLOW_EMPTY_PASSWORD=1 \
		-e MYSQL_ROOT_PASSWORD="" \
		-p 3306:3306 \
		$(MYSQL)

postgres.cid:
	docker run --rm -d \
		--cidfile $@ \
		--name migerl-postgres \
		-e POSTGRES_PASSWORD=$(PASSWORD) \
		-p 5432:5432 \
		$(POSTGRES)

stop:
	$(MAKE) -f docker.mk -j 2 stop-mysql stop-postgres

stop-%:
	[ -f $*.cid ] && docker stop $$(cat $*.cid) || true
	rm -f $*.cid

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
		RET=$$(PGPASSWORD=$(PASSWORD) psql -c "select 1;" -U postgres -h 127.0.0.1 -p 5432); \
		TRIES=$$(($$TRIES - 1)); \
	done || true;
	PGPASSWORD=$(PASSWORD) psql -c "create database migerl_test;" -U postgres -h 127.0.0.1 -p 5432

.PHONY: all pull pull-* start stop stop-* create-db create-mysql create-postgres
