-module(migerl_db_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    Config = migerl_config:load("default", "test/test.config"),
    {mysql, _} = Conn = migerl_db:start(Config),
    ok = migerl_db:stop(Conn).

setup() ->
    Config = migerl_config:load("default", "test/test.config"),
    Conn = migerl_db:start(Config),
    ok = migerl_db:query(Conn, "DROP TABLE IF EXISTS example_table", []),
    Conn.

cleanup(Conn) ->
    migerl_db:stop(Conn).

tx_query_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Conn) ->
        [
            fun() ->
                Query = "CREATE TABLE example_table ( "
                        "id int(11) unsigned not null, "
                        "name varchar(255) not null, "
                        "PRIMARY KEY(id) "
                        ") Engine=InnoDB default charset=utf8;",
                ok = migerl_db:tx_queries(Conn, [{Query, []}])
            end,
            fun() ->
                Queries = [
                    {"INSERT INTO example_table (id, name) VALUES (1, 'hoge');", []},
                    {"INSERT INTO example_table (id, name) VALUES (1, 'fuga');", []}
                ],
                ok = try migerl_db:tx_queries(Conn, Queries) of
                    should_fail -> should_fail
                catch
                    error:_ -> ok
                end
            end,
            fun() ->
                Queries = [
                    {"INSERT INTO example_table (id, name) VALUES (1, 'hoge');", []},
                    {"INSERT INTO example_table (id, name) VALUES (2, 'fuga');", []}
                ],
                done = migerl_db:tx_queries(Conn, Queries),
                {ok, _, Result} = migerl_db:query(Conn, "SELECT * FROM example_table", []),
                ?assertEqual([[1, <<"hoge">>], [2, <<"fuga">>]], Result)
            end
        ]
    end}.
