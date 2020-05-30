DIR := scripts
CONFIG := config
ENV := test

MIGERL := docker run \
	-it \
	--rm \
	-v `pwd`:/work \
	-w /work \
	--network host \
	erlang \
	./migerl -e $(ENV) -c $(CONFIG) -d $(DIR)

all: migerl
	docker pull erlang

migerl: VERSION := v0.3-rc1
migerl:
	curl -L https://github.com/yowcow/migerl/releases/download/$(VERSION)/migerl -o $@ \
		&& chmod +x $@

init:
	$(MIGERL) init

status:
	$(MIGERL) status

new:
	$(MIGERL) -t "$(TITLE)" new

up:
	$(MIGERL) -a up

down:
	$(MIGERL) down

clean:
	rm migerl

.PHONY: all init status new up down clean
