all:

start: mysql.cid

mysql.cid: IMAGE := mysql:5.7
mysql.cid:
	docker run --rm -d \
		--cidfile $@ \
		--name migerl-mysql \
		-e MYSQL_ALLOW_EMPTY_PASSWORD=1 \
		-e MYSQL_ROOT_PASSWORD="" \
		-p 3306:3306 \
		$(IMAGE)

stop:
	[ -f mysql.cid ] && docker stop $$(cat mysql.cid) || true
	rm -f mysql.cid

.PHONY: all start stop
