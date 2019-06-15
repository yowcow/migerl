REBAR := rebar3

all:
	rebar3 escriptize

test:
	$(MAKE) create-db
	rebar3 eunit

clean:
	rm -rf _build

create-db: create-mysql

create-mysql:
	echo "create database if not exists migerl_test default charset=utf8;" \
		| mysql -h127.0.0.1 -uroot -pmigerl

docker-run: mysql.cid

mysql.cid: IMAGE := mysql:5.7
mysql.cid:
	docker run --rm -d \
		--cidfile $@ \
		--name migerl-mysql \
		-e MYSQL_ROOT_PASSWORD=migerl \
		-p 3306:3306 \
		$(IMAGE)

docker-stop:
	[ -f mysql.cid ] && docker stop $$(cat mysql.cid) || true
	rm -f mysql.cid

.PHONY: all test clean create-db create-mysql docker-run docker-stop
