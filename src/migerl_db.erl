-module(migerl_db).

-export([
    start/1,
    stop/1,
    query/3,
    tx_queries/2
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

query({mysql, Pid}, Query, Args) ->
    Ret = mysql:query(Pid, Query, Args),
    case Ret of
        ok         -> ok;
        {ok, _}    -> Ret;
        {ok, _, _} -> Ret;
        Err ->
            error(io_lib:format("mysql query failed: ~p", [Err]))
    end.

tx_queries({mysql, Pid} = Conn, Queries) ->
    try mysql:transaction(Pid, fun() -> queries(Conn, Queries) end) of
        {atomic, Result} ->
            Result
    catch
        error:{implicit_commit, _} -> ok
    end.

queries(_, []) -> done;
queries(Conn, [{Query, Args} | Rem]) ->
    query(Conn, Query, Args),
    queries(Conn, Rem).
