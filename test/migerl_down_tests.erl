-module(migerl_down_tests).

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

setup_mysql() ->
    Conn = setup(migerl_config:load("mysql", ?CONFIG)),
    ok = migerl_init:dispatch(Conn, [{dir, ?MYSQL_SCRIPT_DIR}]),
    ok = migerl_up:dispatch(Conn, [{dir, ?MYSQL_SCRIPT_DIR}, {all, true}]),
    Conn.

setup_postgres() ->
    Conn = setup(migerl_config:load("pg", ?CONFIG)),
    ok = migerl_init:dispatch(Conn, [{dir, ?POSTGRES_SCRIPT_DIR}]),
    ok = migerl_up:dispatch(Conn, [{dir, ?POSTGRES_SCRIPT_DIR}, {all, true}]),
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
                "unapply 1",
                fun() ->
                    ok = migerl_down:dispatch(Conn, Opts),
                    {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM member", []),
                    ?assertEqual(0, Count)
                end
            },
            {
                "unapply all",
                fun() ->
                    ok = migerl_down:dispatch(Conn, [{all, true} | Opts]),
                    {ok, _, []} = migerl_db:query(Conn, "SHOW TABLES LIKE '%member%'", [])
                end
            }
        ]
    end}.

dispatch_postgres_test_() ->
    Opts = [{dir, ?POSTGRES_SCRIPT_DIR}, {all, false}],
    {setup, fun setup_postgres/0, fun cleanup/1, fun(Conn) ->
        [
            {
                "unapply 1",
                fun() ->
                    {ok, _, Res1} = migerl_db:query(Conn, "select table_schema, table_name from information_schema.tables", []),
                    erlang:display(Res1),
                    ok = migerl_down:dispatch(Conn, Opts),
                    {ok, _, [[Count]]} = migerl_db:query(Conn, "SELECT count(*) FROM member", []),
                    ?assertEqual(0, Count)
                end
            },
            {
                "unapply all",
                fun() ->
                    ok = migerl_down:dispatch(Conn, [{all, true} | Opts]),
                    {ok, _, [[1]]} = migerl_db:query(
                        Conn,
                        "select 1 "
                        "from information_schema.tables "
                        "where table_schema = 'public' and table_name = 'migrations'",
                        []
                    ),
                    {ok, _, []} = migerl_db:query(
                        Conn,
                        "select 1 "
                        "from information_schema.tables "
                        "where table_schema = 'public' and table_name = 'member'",
                        []
                    ),
                    {ok, _, []} = migerl_db:query(
                        Conn,
                        "select 1 "
                        "from information_schema.tables "
                        "where table_schema = 'public' and table_name = 'member_password'",
                        []
                    )
                end
            }
        ]
    end}.
