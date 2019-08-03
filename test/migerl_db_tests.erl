-module(migerl_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

start_stop_test_() ->
    [
        fun() ->
            Config = migerl_config:load("mysql", ?CONFIG),
            {mysql, _} = Conn = migerl_db:start(Config),
            ok = migerl_db:stop(Conn)
        end,
        fun() ->
            Config = migerl_config:load("pg", ?CONFIG),
            {postgres, _} = Conn = migerl_db:start(Config),
            ok = migerl_db:stop(Conn)
        end
    ].

setup_mysql() ->
    setup(migerl_config:load("mysql", ?CONFIG)).

setup_postgres() ->
    setup(migerl_config:load("pg", ?CONFIG)).

setup(Config) ->
    Conn = migerl_db:start(Config),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS migrations", []),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS example_table", []),
    ok = migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-hoge"}]),
    Conn.

cleanup(Conn) ->
    migerl_db:stop(Conn).

notx_tx_queries_mysql_test_() ->
    {setup, fun setup_mysql/0, fun cleanup/1, fun(Conn) ->
        [
            {
                "create table without transaction",
                fun() ->
                    Query = "CREATE TABLE example_table ( "
                            "id int(11) unsigned not null, "
                            "name varchar(255) not null, "
                            "PRIMARY KEY(id) "
                            ") Engine=InnoDB default charset=utf8;",
                    ok = migerl_db:queries(Conn, [{Query, []}])
                end
            },
            {
                "insert with dupe primary key in transaction",
                fun() ->
                    Queries = [
                        {"INSERT INTO example_table (id, name) VALUES (1, 'hoge');", []},
                        {"INSERT INTO example_table (id, name) VALUES (1, 'fuga');", []}
                    ],
                    ok = try migerl_db:tx_queries(Conn, Queries) of
                        should_fail -> should_fail
                    catch
                        _:_ -> ok
                    end
                end
            },
            {
                "insert succeeds in transaction",
                fun() ->
                    Queries = [
                        {"INSERT INTO example_table (id, name) VALUES (1, 'hoge');", []},
                        {"INSERT INTO example_table (id, name) VALUES (2, 'fuga');", []}
                    ],
                    ok = migerl_db:tx_queries(Conn, Queries),
                    {ok, _, Result} = migerl_db:query(Conn, "SELECT * FROM example_table", []),
                    ?assertEqual([[1, <<"hoge">>], [2, <<"fuga">>]], Result)
                end
            }
        ]
    end}.

notx_tx_queries_postgres_test_() ->
    {setup, fun setup_postgres/0, fun cleanup/1, fun(Conn) ->
        [
            {
                "create table without transaction",
                fun() ->
                    Query = "CREATE TABLE example_table ( "
                            "id integer not null, "
                            "name text not null, "
                            "PRIMARY KEY(id) "
                            ")",
                    ok = migerl_db:queries(Conn, [{Query, []}])
                end
            },
            {
                "insert with dupe primary key in transaction",
                fun() ->
                    Queries = [
                        {"INSERT INTO example_table (id, name) VALUES (1, 'hoge');", []},
                        {"INSERT INTO example_table (id, name) VALUES (1, 'fuga');", []}
                    ],
                    ok = try migerl_db:tx_queries(Conn, Queries) of
                        should_fail -> should_fail
                    catch
                        _:_ -> ok
                    end
                end
            },
            {
                "insert succeeds in transaction",
                fun() ->
                    Queries = [
                        {"INSERT INTO example_table (id, name) VALUES (1, 'hoge');", []},
                        {"INSERT INTO example_table (id, name) VALUES (2, 'fuga');", []}
                    ],
                    ok = migerl_db:tx_queries(Conn, Queries),
                    {ok, _, Result} = migerl_db:query(Conn, "SELECT * FROM example_table", []),
                    ?assertEqual([[1, <<"hoge">>], [2, <<"fuga">>]], Result)
                end
            }
        ]
    end}.

apply_unapply_mysql_test_() ->
    {setup, fun setup_mysql/0, fun cleanup/1, fun(Conn) ->
        File = "hoge_test",
        [
            fun() ->
                ?assertEqual(false, migerl_db:is_applied(Conn, File))
            end,
            fun() ->
                {Query, Args} = migerl_db:apply_query(Conn, File),
                ok = migerl_db:query(Conn, Query, Args)
            end,
            fun() ->
                ?assertEqual(true, migerl_db:is_applied(Conn, File))
            end,
            fun() ->
                {Query, Args} = migerl_db:unapply_query(Conn, File),
                ok = migerl_db:query(Conn, Query, Args)
            end,
            fun() ->
                ?assertEqual(false, migerl_db:is_applied(Conn, File))
            end
        ]
    end}.

apply_unapply_postgres_test_() ->
    {setup, fun setup_postgres/0, fun cleanup/1, fun(Conn) ->
        File = "hoge_test",
        [
            fun() ->
                ?assertEqual(false, migerl_db:is_applied(Conn, File))
            end,
            fun() ->
                {Query, Args} = migerl_db:apply_query(Conn, File),
                {ok, 1} = migerl_db:query(Conn, Query, Args)
            end,
            fun() ->
                ?assertEqual(true, migerl_db:is_applied(Conn, File))
            end,
            fun() ->
                {Query, Args} = migerl_db:unapply_query(Conn, File),
                {ok, 1} = migerl_db:query(Conn, Query, Args)
            end,
            fun() ->
                ?assertEqual(false, migerl_db:is_applied(Conn, File))
            end
        ]
    end}.

get_status_mysql_test_() ->
    {setup, fun setup_mysql/0, fun cleanup/1, fun(Conn) ->
        [
            {
                "all unapplied",
                fun() ->
                    Files = migerl_util:list_dir(?MYSQL_SCRIPT_DIR),
                    Actual = migerl_db:get_status(Conn, Files),
                    Expected = [
                        {"file1.sql", ?MYSQL_SCRIPT_DIR++"/file1.sql", will_be_applied},
                        {"file2.sql", ?MYSQL_SCRIPT_DIR++"/file2.sql", will_be_applied},
                        {"file3.sql", ?MYSQL_SCRIPT_DIR++"/file3.sql", will_be_applied}
                    ],
                    ?assertEqual(Expected, Actual)
                end
            },
            {
                "applied migration in the middle",
                fun() ->
                    {Query, Args} = migerl_db:apply_query(Conn, "file2.sql"),
                    ok = migerl_db:query(Conn, Query, Args),
                    Files = migerl_util:list_dir(?MYSQL_SCRIPT_DIR),
                    Actual = migerl_db:get_status(Conn, Files),
                    ?assertMatch([
                        {"file1.sql", ?MYSQL_SCRIPT_DIR++"/file1.sql", wont_be_applied},
                        {"file2.sql", ?MYSQL_SCRIPT_DIR++"/file2.sql", {_, _}},
                        {"file3.sql", ?MYSQL_SCRIPT_DIR++"/file3.sql", will_be_applied}
                    ], Actual)
                end
            }
        ]
    end}.

get_status_postgres_test_() ->
    {setup, fun setup_postgres/0, fun cleanup/1, fun(Conn) ->
        [
            {
                "all unapplied",
                fun() ->
                    Files = migerl_util:list_dir(?MYSQL_SCRIPT_DIR),
                    Actual = migerl_db:get_status(Conn, Files),
                    Expected = [
                        {"file1.sql", ?MYSQL_SCRIPT_DIR++"/file1.sql", will_be_applied},
                        {"file2.sql", ?MYSQL_SCRIPT_DIR++"/file2.sql", will_be_applied},
                        {"file3.sql", ?MYSQL_SCRIPT_DIR++"/file3.sql", will_be_applied}
                    ],
                    ?assertEqual(Expected, Actual)
                end
            },
            {
                "applied migration in the middle",
                fun() ->
                    {Query, Args} = migerl_db:apply_query(Conn, "file2.sql"),
                    {ok, 1} = migerl_db:query(Conn, Query, Args),
                    Files = migerl_util:list_dir(?MYSQL_SCRIPT_DIR),
                    Actual = migerl_db:get_status(Conn, Files),
                    ?assertMatch([
                        {"file1.sql", ?MYSQL_SCRIPT_DIR++"/file1.sql", wont_be_applied},
                        {"file2.sql", ?MYSQL_SCRIPT_DIR++"/file2.sql", {_, _}},
                        {"file3.sql", ?MYSQL_SCRIPT_DIR++"/file3.sql", will_be_applied}
                    ], Actual)
                end
            }
        ]
    end}.
