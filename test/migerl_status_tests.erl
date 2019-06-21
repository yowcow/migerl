-module(migerl_status_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    Config = migerl_config:load("default", "test/test.config"),
    Conn = migerl_db:start(Config),
    ok = migerl_db:query(Conn, "DROP TABLE IF EXISTS member_password", []),
    ok = migerl_db:query(Conn, "DROP TABLE IF EXISTS member", []),
    ok = migerl_db:query(Conn, "DROP TABLE IF EXISTS migrations", []),
    ok = migerl_init:dispatch(Conn, [{dir, "test/files"}]),
    Conn.

cleanup(Conn) ->
    migerl_db:stop(Conn).

dispatch_test_() ->
    Opts = [{dir, "test/files"}],
    {setup, fun setup/0, fun cleanup/1, fun(Conn) ->
        [
            {
                "nothing applied",
                fun() ->
                    ok = migerl_status:dispatch(Conn, Opts)
                end
            }%,
            %{
            %    "unapply all",
            %    fun() ->
            %        ok = migerl_down:dispatch(Conn, [{all, true} | Opts]),
            %        {ok, _, []} = migerl_db:query(Conn, "SHOW TABLES LIKE '%member%'", [])
            %    end
            %}
        ]
    end}.
