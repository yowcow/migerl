-module(migerl_db).

-export([
    start/1,
    stop/1,
    query/2
]).

start(Config) ->
    start(proplists:get_value(dialect, Config), Config).

start(mysql, Config) ->
    {ok, Pid} = mysql:start_link(Config),
    {mysql, Pid};
start(Dialect, _) ->
    error("unknown dialect: " ++ atom_to_list(Dialect)).

stop({mysql, Pid}) ->
    unlink(Pid),
    mysql:stop(Pid).

query({mysql, Pid}, {Stmt, Args}) ->
    Ret = mysql:query(Pid, Stmt, Args),
    case Ret of
        ok         -> ok;
        {ok, _}    -> Ret;
        {ok, _, _} -> Ret;
        Err ->
            error(io_lib:format("mysql query failed: ~p", [Err]))
    end.
