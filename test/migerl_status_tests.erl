-module(migerl_status_tests).

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

setup_mysql() ->
    Ctx = {Conn, _} = setup(migerl_config:load("mysql", ?CONFIG)),
    ok = migerl_init:dispatch(Conn, [{dir, ?MYSQL_SCRIPT_DIR}]),
    Ctx.

setup_postgres() ->
    Ctx = {Conn, _} = setup(migerl_config:load("pg", ?CONFIG)),
    ok = migerl_init:dispatch(Conn, [{dir, ?POSTGRES_SCRIPT_DIR}]),
    Ctx.

setup(Config) ->
    Conn = migerl_db:start(Config),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS member_password", []),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS member", []),
    _ = migerl_db:query(Conn, "DROP TABLE IF EXISTS migrations", []),
    {ok, Apps} = application:ensure_all_started(migerl),
    {Conn, Apps}.

cleanup({Conn, Apps}) ->
    error_logger:tty(false),
    try [application:stop(App) || App <- Apps]
    after error_logger:tty(true)
    end,
    migerl_db:stop(Conn).

dispatch_mysql_test_() ->
    Opts = [{dir, ?MYSQL_SCRIPT_DIR}],
    {setup,
     fun setup_mysql/0,
     fun cleanup/1,
     fun({Conn, _}) ->
             [
              {
               "nothing applied",
               fun() ->
                       ok = migerl_status:dispatch(Conn, Opts)
               end
              },
              {
               "apply all",
               fun() ->
                       ok = migerl_up:dispatch(Conn, [{all, true} | Opts]),
                       ok = migerl_status:dispatch(Conn, Opts)
               end
              }
             ]
     end
    }.

dispatch_postgres_test_() ->
    Opts = [{dir, ?POSTGRES_SCRIPT_DIR}],
    {setup,
     fun setup_postgres/0,
     fun cleanup/1,
     fun({Conn, _}) ->
             [
              {
               "nothing applied",
               fun() ->
                       ok = migerl_status:dispatch(Conn, Opts)
               end
              },
              {
               "apply all",
               fun() ->
                       ok = migerl_up:dispatch(Conn, [{all, true} | Opts]),
                       ok = migerl_status:dispatch(Conn, Opts)
               end
              }
             ]
     end
    }.
