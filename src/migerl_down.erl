-module(migerl_down).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    All = proplists:get_value(all, Opts),
    unapply_migrations(Conn, lists:reverse(Files), All).

unapply_migrations(_, [], _) ->
    migerl_util:log_info("no more migrations to unapply!"),
    ok;
unapply_migrations(Conn, [{Name, Path} | Rem], false) ->
    case migerl_db:is_applied(Conn, Name) of
        true ->
            Content = migerl_util:read_file(Path),
            unapply_one(Conn, Name, migerl_util:read_down(Content));
        _ ->
            unapply_migrations(Conn, Rem, false)
    end;
unapply_migrations(Conn, [{Name, Path} | Rem], true) ->
    case migerl_db:is_applied(Conn, Name) of
        true ->
            Content = migerl_util:read_file(Path),
            unapply_one(Conn, Name, migerl_util:read_down(Content)),
            unapply_migrations(Conn, Rem, true);
        _ ->
            unapply_migrations(Conn, Rem, true)
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
