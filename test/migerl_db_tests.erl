-module(migerl_db_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    Config = migerl_config:load("default", "test/test.config"),
    {mysql, _} = Conn = migerl_db:start(Config),
    ok = migerl_db:stop(Conn).

setup() ->
    Config = migerl_config:load("default", "test/test.config"),
    Conn = migerl_db:start(Config),
    ok = migerl_db:query(Conn, "DROP TABLE IF EXISTS migrations", []),
    ok = migerl_db:query(Conn, "DROP TABLE IF EXISTS example_table", []),
    ok = migerl_init:dispatch(Conn, [{dir, "/tmp/migerl-hoge"}]),
    Conn.

cleanup(Conn) ->
    migerl_db:stop(Conn).

notx_tx_queries_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Conn) ->
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
                        error:_ ->
                            ok
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

apply_unapply_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Conn) ->
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
