MYSQL := mysql:5.7
POSTGRES := postgres:11

all:
	$(MAKE) -f docker.mk -j 2 pull-$(MYSQL) pull-$(POSTGRES)

pull-%:
	docker pull $*

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
	$(MAKE) -f docker.mk -j 2 stop-mysql.cid stop-postgres.cid

stop-%:
	[ -f $* ] && docker stop $$(cat $*) || true
	rm -f $*

.PHONY: all start stop
