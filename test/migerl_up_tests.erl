-module(migerl_up_tests).

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

setup_mysql() ->
    setup(migerl_config:load("mysql", ?CONFIG)).

setup(Config) ->
    Conn = migerl_db:start(Config),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS member_password", []),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS member", []),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS migrations", []),
    ok = migerl_init:dispatch(Conn, [{dir, ?MYSQL_SCRIPT_DIR}]),
    Conn.

cleanup(Conn) ->
    migerl_db:stop(Conn).

dispatch_test_() ->
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
