-module(migerl_init_tests).

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

setup_mysql() ->
    setup(migerl_config:load("mysql", ?CONFIG)).

setup_postgres() ->
    setup(migerl_config:load("pg", ?CONFIG)).

setup(Config) ->
    Conn = migerl_db:start(Config),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS migrations", []),
    Conn.

cleanup(Conn) ->
    migerl_db:stop(Conn).

dispatch_mysql_test_() ->
    {setup,
     fun setup_mysql/0,
     fun cleanup/1,
     fun(Conn) ->
             [
              fun() ->
                      ok = migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-hoge"}]),
                      {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM migrations", []),
                      ?assertEqual(0, Count)
              end,
              fun() ->
                      ok = migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-hoge"}]),
                      {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM migrations", []),
                      ?assertEqual(0, Count)
              end,
              fun() ->
                      ?assertError(
                         enoent,
                         migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-fuga/hoge"}])
                        )
              end
             ]
     end
    }.

dispatch_postgres_test_() ->
    {setup,
     fun setup_postgres/0,
     fun cleanup/1,
     fun(Conn) ->
             [
              fun() ->
                      ok = migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-hoge"}]),
                      {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM migrations", []),
                      ?assertEqual(0, Count)
              end,
              fun() ->
                      ok = migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-hoge"}]),
                      {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM migrations", []),
                      ?assertEqual(0, Count)
              end,
              fun() ->
                      ?assertError(
                         enoent,
                         migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-fuga/hoge"}])
                        )
              end
             ]
     end
    }.
