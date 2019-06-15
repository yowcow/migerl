-module(migerl_db_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    Config = migerl_config:load("default", "test/test.config"),
    {mysql, _} = Conn = migerl_db:start(Config),
    ok = migerl_db:stop(Conn).
