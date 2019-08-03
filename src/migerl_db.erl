-module(migerl_db).

-export([
    start/1,
    stop/1,
    query/3,
    queries/2,
    tx_queries/2,

    create_table/1,
    get_applied_at/2,
    is_applied/2,
    get_status/2,

    apply_query/2,
    unapply_query/2
]).

start(Data) ->
    start(proplists:get_value(dialect, Data), proplists:get_value(config, Data)).

start(mysql, Config) ->
    case mysql:start_link(Config) of
        {ok, Pid} -> {mysql, Pid};
        Error -> migerl_util:log_error("failed connecting to mysql", Error)
    end;
start(postgres, Config) ->
    case epgsql:connect(Config) of
        {ok, Pid} -> {postgres, Pid};
        Error -> migerl_util:log_error("failed connecting to postgres", Error)
    end;
start(Dialect, _) ->
    migerl_util:log_error("unknown dialect", Dialect).

stop({mysql, Pid}) ->
    unlink(Pid),
    mysql:stop(Pid);
stop({postgres, Pid}) ->
    unlink(Pid),
    epgsql:close(Pid).

query({mysql, Pid}, Query, Args) ->
    Ret = mysql:query(Pid, Query, Args),
    Result = case Ret of
        ok         -> ok;
        {ok, _}    -> Ret;
        {ok, _, _} -> Ret;
        Err ->
            migerl_util:log_error("mysql query failed", Err)
    end,
    Result;
query({postgres, Pid}, Query, Args) ->
    Ret = epgsql:equery(Pid, Query, Args),
    Result = case Ret of
        ok -> ok;
        {ok, _} -> Ret;
        {ok, Cols, Rows} ->
            {ok, Cols, [tuple_to_list(Row) || Row <- Rows]};
        Err ->
            migerl_util:log_error("postgres query failed", Err)
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
        _:Err ->
            throw(Err)
    end;
tx_queries({postgres, Pid}, Queries) ->
    try epgsql:with_transaction(Pid, fun(C) -> queries({postgres, C}, Queries) end, [{reraise, true}]) of
        Result -> Result
    catch
        _:Err ->
            throw(Err)
    end.

create_table(Conn) ->
    {Query, Args} = create_table_query(Conn),
    migerl_db:query(Conn, Query, Args),
    ok.


get_applied_at(Conn, Name) ->
    {Query, Args} = get_applied_at_query(Conn, Name),
    {ok, _, Result} = query(Conn, Query, Args),
    case Result of
        [[Timestamp]] -> Timestamp;
        _ -> undefined % []
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


%%--- queries defined for dialects ---

create_table_query({mysql, _}) ->
    {
        "CREATE TABLE IF NOT EXISTS `migrations` ("
        "`id` varchar(254) NOT NULL, "
        "`applied_at` datetime DEFAULT NULL, "
        "PRIMARY KEY (`id`) "
        ") ENGINE=InnoDB DEFAULT CHARSET=utf8",
        []
    };
create_table_query({postgres, _}) ->
    {
        "CREATE TABLE IF NOT EXISTS migrations ("
        "id TEXT NOT NULL, "
        "applied_at TIMESTAMP DEFAULT NULL, "
        "PRIMARY KEY (id) "
        ")",
        []
    }.

get_applied_at_query({mysql, _}, Name) ->
    {
        "SELECT applied_at FROM migrations "
        "WHERE id = ? AND applied_at IS NOT NULL",
        [Name]
    };
get_applied_at_query({postgres, _}, Name) ->
    {
        "SELECT applied_at FROM migrations "
        "WHERE id = $1 AND applied_at IS NOT NULL",
        [Name]
    }.

apply_query({mysql, _}, Name) ->
    {
        "INSERT INTO migrations "
        "SET id = ?, applied_at = NOW() "
        "ON DUPLICATE KEY UPDATE applied_at = NOW() ",
        [Name]
    };
apply_query({postgres, _}, Name) ->
    {
        "INSERT INTO migrations (id, applied_at) VALUES ($1, NOW()) "
        "ON CONFLICT (id) DO UPDATE SET applied_at = NOW() ",
        [Name]
    }.

unapply_query({mysql, _}, Name) ->
    {
        "DELETE FROM migrations "
        "WHERE id = ?",
        [Name]
    };
unapply_query({postgres, _}, Name) ->
    {
        "DELETE FROM migrations "
        "WHERE id = $1",
        [Name]
    }.
