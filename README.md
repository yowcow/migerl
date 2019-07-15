[![Build Status](https://travis-ci.org/yowcow/migerl.svg?branch=master)](https://travis-ci.org/yowcow/migerl)

migerl
======

Yet another database migration tool written in Erlang.


What it does
------------

* Manages applied/unapplied migrations
* Runs a migration with multiple queries under a transaction whenever possible


How to install
--------------

1. With Erlang/OTP runtime, download release binary from [releases](https://github.com/yowcow/migerl/releases).
2. Without Erlang/OTP runtime, pull docker image with `docker pull yowcow/migerl`.


How to use
----------

```

Usage: migerl [-c <config>] [-e [<env>]] [-d [<dir>]] [-t [<title>]]
              [-a [<all>]] [-v [<version>]] [-h [<help>]] [command ...]

  -c, --config   Config file
  -e, --env      Env name in config [default: default]
  -d, --dir      Migration script directory [default: scripts]
  -t, --title    Migration title [default: untitled]
  -a, --all      Apply/Unapply all [default: false]
  -v, --version  Print version [default: false]
  -h, --help     Print this help [default: false]
  command        Commands to execute (e.g. init, new, status, up, down)

```

**Initialize:**

    migerl -c path/to/config init

**Create a new migration file:**

    migerl -c path/to/config -t the_new_migration new

**Show current status:**

    migerl -c path/to/config status

**Apply 1 migration:**

    migerl -c path/to/config up

**Undo 1 migration:**

    migerl -c path/to/config down

**Apply all unapplied migrations:**

    migerl -c path/to/config -a up

**Undo all applied migrations:**

    migerl -c path/to/config -a down


Configuration file
------------------

Should be in erlang proplist:

```
[
  {"default", [
    {dialect, mysql},
    {host, "127.0.0.1"},
    {port, 3306},
    {user, "test-migration-user"},
    {password, "test-user-p@ssw0rd"},
    {database, "test_db"},
    {queries, [
      "SET NAMES utf8mb4"
    ]}
  ]},
  {"devel", [
    {dialect, mysql},
    {host, "devel-db-host"},
    {port, 3306},
    {user, "devel-migration-user"},
    {password, "devel-user-p@ssw0rd"},
    {database, "devel_db"},
    {queries, [
      "SET NAMES utf8mb4"
    ]}
  ]},
  {"prod", [
    {dialect, mysql},
    {host, "prod-db-host"},
    {port, 3306},
    {user, "prod-migration-user"},
    {password, "devel-user-p@ssw0rd"},
    {database, "prod_db"},
    {queries, [
      "SET NAMES utf8mb4"
    ]}
  ]}
].
```


Build
-----

    rebar3 escriptize


Limitations
-----------

* Only MySQL is supported at the moment
