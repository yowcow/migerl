MYSQL := mysql:5.7
POSTGRES := postgres:9.6

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
		-e POSTGRES_PASSWORD="" \
		-p 5432:5432 \
		$(POSTGRES)

stop:
	$(MAKE) -f docker.mk -j 2 stop-mysql stop-postgres

stop-%:
	[ -f $*.cid ] && docker stop $$(cat $*.cid) || true
	rm -f $*.cid

.PHONY: all pull pull-* start stop stop-*
