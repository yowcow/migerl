migerl example
==============

Try and see how it works!

Prerequisite
------------

* make
* docker
* curl

Have MySQL started, and have databases created:

    make -C ../ -f docker.mk start create-db

    echo "create database mytest_db default charset=utf8mb4;" | mysql -h 127.0.0.2 -u root

How it works
------------

    make all # prepare

    make init # initialize

    make status # check migration progress

    make up # apply all scirpts

    make down # unapply 1 script

Example
-------

    make all init up status
