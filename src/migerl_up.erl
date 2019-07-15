-module(migerl_up).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    Migrations = migerl_db:get_status(Conn, Files),
    All = proplists:get_value(all, Opts),
    apply_migrations(Conn, Migrations, All).

apply_migrations(_, [], _) ->
    migerl_util:log_info("-> no more migrations to apply!"),
    ok;
apply_migrations(Conn, [{Name, Path, will_be_applied} | Rem], All) ->
    Content = migerl_util:read_file(Path),
    apply_one(Conn, Name, migerl_util:read_up(Content)),
    case All of
        true -> apply_migrations(Conn, Rem, true);
        _ -> ok
    end;
apply_migrations(Conn, [{_, _, _} | Rem], All) ->
    apply_migrations(Conn, Rem, All).

apply_one(_, Name, undefined) ->
    migerl_util:log_error("failed applying "++Name, "no up query to apply!");
apply_one(Conn, Name, {Tx, Queries}) ->
    BoundQueries = [{Q , []} || Q <- Queries] ++ [migerl_db:apply_query(Conn, Name)],
    case Tx of
        notx ->
            migerl_util:log_info("-> applying "++Name++" without tx..."),
            migerl_util:log_queries(Queries),
            migerl_db:queries(Conn, BoundQueries);
        _ ->
            migerl_util:log_info("-> applying "++Name++" with tx..."),
            migerl_util:log_queries(Queries),
            migerl_db:tx_queries(Conn, BoundQueries)
    end.
