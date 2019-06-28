-module(migerl_down).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    Migrations = migerl_db:get_status(Conn, Files),
    All = proplists:get_value(all, Opts),
    unapply_migrations(Conn, lists:reverse(Migrations), All),
    migerl_status:dispatch(Conn, Opts).

unapply_migrations(_, [], _) ->
    migerl_util:log_info("no more migrations to unapply!"),
    ok;
unapply_migrations(Conn, [{_, _, wont_be_applied} | Rem], All) ->
    unapply_migrations(Conn, Rem, All);
unapply_migrations(Conn, [{_, _, will_be_applied} | Rem], All) ->
    unapply_migrations(Conn, Rem, All);
unapply_migrations(Conn, [{Name, Path, _} | Rem], All) ->
    Content = migerl_util:read_file(Path),
    unapply_one(Conn, Name, migerl_util:read_down(Content)),
    case All of
        true -> unapply_migrations(Conn, Rem, All);
        _ -> ok
    end.

unapply_one(Conn, Name, undefined) ->
    migerl_util:log_info("unapplying "++Name++" but it has no query to run!"),
    {Query, Args} = migerl_db:unapply_query(Conn, Name),
    migerl_db:query(Conn, Query, Args);
unapply_one(Conn, Name, {Tx, Queries0}) ->
    Queries = [{Q , []} || Q <- Queries0] ++ [migerl_db:unapply_query(Conn, Name)],
    case Tx of
        notx ->
            migerl_util:log_info("unapplying "++Name++" w/o tx!"),
            migerl_db:queries(Conn, Queries);
        _ ->
            migerl_util:log_info("unapplying "++Name++" with tx!"),
            migerl_db:tx_queries(Conn, Queries)
    end.
