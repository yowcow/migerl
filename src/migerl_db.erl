-module(migerl_db).

-export([
    start/1,
    stop/1,
    query/3,
    queries/2,
    tx_queries/2,
    get_applied_at/2,
    is_applied/2,
    get_status/2,
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

get_applied_at(Conn, Name) ->
    {ok, _, Result} = query(
        Conn,
        "SELECT applied_at FROM migrations "
        "WHERE id = ? AND applied_at IS NOT NULL",
        [Name]
    ),
    case Result of
        [[Timestamp]] ->
            Timestamp;
        _ -> % []
            undefined
    end.

is_applied(Conn, Name) ->
    case get_applied_at(Conn, Name) of
        undefined -> false;
        _ -> true
    end.

get_status(Conn, Files) ->
    get_status(Conn, lists:reverse(Files), true, []).

get_status(_, [], _, Acc) -> Acc;
get_status(Conn, [{Name, Path} | Rem], ToBeApplied, Acc) ->
    case get_applied_at(Conn, Name) of
        undefined ->
            case ToBeApplied of
                true ->
                    get_status(Conn, Rem, true, [{Name, Path, will_be_applied} | Acc]);
                false ->
                    get_status(Conn, Rem, false, [{Name, Path, wont_be_applied} | Acc])
            end;
        Timestamp ->
            get_status(Conn, Rem, false, [{Name, Path, Timestamp} | Acc])
    end.

apply_query({mysql, _}, Name) ->
    {
        "INSERT INTO migrations "
        "SET id = ?, applied_at = NOW() "
        "ON DUPLICATE KEY UPDATE applied_at = NOW() ",
        [Name]
    }.

unapply_query(_, Name) ->
    {
        "DELETE FROM migrations "
        "WHERE id = ?",
        [Name]
    }.
