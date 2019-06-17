-module(migerl_db).

-export([
    start/1,
    stop/1,
    query/3,
    queries/2,
    tx_queries/2,
    is_applied/2,
    apply_query/2,
    unapply_query/2
]).

start(Config) ->
    start(proplists:get_value(dialect, Config), Config).

start(mysql, Config) ->
    case mysql:start_link(Config) of
        {ok, Pid} -> {mysql, Pid};
        Error -> migerl_util:log_error("failed connecting to mysql", Error)
    end;
start(Dialect, _) ->
    migerl_util:log_error("unknown dialect", Dialect).

stop({mysql, Pid}) ->
    unlink(Pid),
    mysql:stop(Pid).

query({mysql, Pid}, Query, Args) ->
    Ret = mysql:query(Pid, Query, Args),
    Result = case Ret of
        ok         -> ok;
        {ok, _}    -> Ret;
        {ok, _, _} -> Ret;
        Err ->
            migerl_util:log_error("mysql query failed", Err)
    end,
    Result.

queries(_, []) -> ok;
queries(Conn, [{Query, Args} | Rem]) ->
    query(Conn, Query, Args),
    queries(Conn, Rem).

tx_queries({mysql, Pid} = Conn, Queries) ->
    try mysql:transaction(Pid, fun() -> queries(Conn, Queries) end) of
        {atomic, Result} -> Result
    catch
        error:Err ->
            throw(Err)
    end.

is_applied(Conn, File) ->
    {ok, _, [[Count]]} = query(
        Conn,
        "SELECT count(*) FROM migrations "
        "WHERE id = ? AND applied_at IS NOT NULL",
        [File]
    ),
    case Count of
        0 -> false;
        _ -> true
    end.

apply_query({mysql, _}, File) ->
    {
        "INSERT INTO migrations "
        "SET id = ?, applied_at = NOW() "
        "ON DUPLICATE KEY UPDATE applied_at = NOW() ",
        [File]
    }.

unapply_query(_, File) ->
    {
        "DELETE FROM migrations "
        "WHERE id = ?",
        [File]
    }.
