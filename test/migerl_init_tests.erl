-module(migerl_init_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    Config = migerl_config:load("default", "test/valid.config"),
    Conn = migerl_db:start(Config),
    ok = migerl_db:query(Conn, {"DROP TABLE IF EXISTS migrations", []}),
    Conn.

cleanup(Conn) ->
    migerl_db:stop(Conn).

dispatch_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Conn) ->
        [
            fun() ->
                ok = migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-hoge"}]),
                {ok, _, [[Count]]} = migerl_db:query(Conn, {"SELECT count(*) FROM migrations", []}),
                ?assertEqual(0, Count)
            end,
            fun() ->
                ok = migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-hoge"}]),
                {ok, _, [[Count]]} = migerl_db:query(Conn, {"SELECT count(*) FROM migrations", []}),
                ?assertEqual(0, Count)
            end,
            fun() ->
                ?assertError(
                    "failed creating dir /tmp/migerl-fuga/hoge: enoent",
                    migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-fuga/hoge"}])
                )
            end
        ]
    end}.
