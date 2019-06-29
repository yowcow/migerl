MYSQL := mysql:5.7

all:
	docker pull $(MYSQL)

start: mysql.cid

mysql.cid:
	docker run --rm -d \
		--cidfile $@ \
		--name migerl-mysql \
		-e MYSQL_ALLOW_EMPTY_PASSWORD=1 \
		-e MYSQL_ROOT_PASSWORD="" \
		-p 3306:3306 \
		$(MYSQL)

stop:
	[ -f mysql.cid ] && docker stop $$(cat mysql.cid) || true
	rm -f mysql.cid

.PHONY: all start stop
