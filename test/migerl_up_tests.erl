-module(migerl_up_tests).

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

setup_mysql() ->
    Conn = setup(migerl_config:load("mysql", ?CONFIG)),
    ok = migerl_init:dispatch(Conn, [{dir, ?MYSQL_SCRIPT_DIR}]),
    Conn.

setup_postgres() ->
    Conn = setup(migerl_config:load("pg", ?CONFIG)),
    ok = migerl_init:dispatch(Conn, [{dir, ?POSTGRES_SCRIPT_DIR}]),
    Conn.

setup(Config) ->
    Conn = migerl_db:start(Config),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS member_password", []),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS member", []),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS migrations", []),
    Conn.

cleanup(Conn) ->
    migerl_db:stop(Conn).

dispatch_mysql_test_() ->
    Opts = [{dir, ?MYSQL_SCRIPT_DIR}, {all, false}],
    {setup, fun setup_mysql/0, fun cleanup/1, fun(Conn) ->
        [
            {
                "apply 1",
                fun() ->
                    ok = migerl_up:dispatch(Conn, Opts),
                    {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM member", []),
                    ?assertEqual(0, Count)
                end
            },
            {
                "apply all",
                fun() ->
                    ok = migerl_up:dispatch(Conn, [{all, true} | Opts]),
                    {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM member_password", []),
                    ?assertEqual(3, Count)
                end
            }
        ]
    end}.

dispatch_postgres_test_() ->
    Opts = [{dir, ?POSTGRES_SCRIPT_DIR}, {all, false}],
    {setup, fun setup_postgres/0, fun cleanup/1, fun(Conn) ->
        [
            {
                "apply 1",
                fun() ->
                    ok = migerl_up:dispatch(Conn, Opts),
                    {ok, _, Res1} = migerl_db:query(Conn, "select table_schema, table_name from information_schema.tables order by table_schema", []),
                    erlang:display({res1, Res1}),
                    {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM member", []),
                    ?assertEqual(0, Count)
                end
            },
            {
                "apply all",
                fun() ->
                    ok = migerl_up:dispatch(Conn, [{all, true} | Opts]),
                    {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM member_password", []),
                    ?assertEqual(3, Count)
                end
            }
        ]
    end}.
