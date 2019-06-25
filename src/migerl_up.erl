-module(migerl_up).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    All = proplists:get_value(all, Opts),
    apply_migrations(Conn, Files, All).

apply_migrations(_, [], _) ->
    migerl_util:log_info("no more migrations to apply!"),
    ok;
apply_migrations(Conn, [{Name, Path} | Rem], false) ->
    case migerl_db:is_applied(Conn, Name) of
        false ->
            Content = migerl_util:read_file(Path),
            apply_one(Conn, Name, migerl_util:read_up(Content));
        _ ->
            apply_migrations(Conn, Rem, false)
    end;
apply_migrations(Conn, [{Name, Path} | Rem], true) ->
    case migerl_db:is_applied(Conn, Name) of
        false ->
            Content = migerl_util:read_file(Path),
            apply_one(Conn, Name, migerl_util:read_up(Content)),
            apply_migrations(Conn, Rem, true);
        _ ->
            apply_migrations(Conn, Rem, true)
    end.

apply_one(_, Name, undefined) ->
    migerl_util:log_error("failed applying "++Name, "no up query to apply");
apply_one(Conn, Name, {Tx, Queries0}) ->
    Queries = [{Q , []} || Q <- Queries0] ++ [migerl_db:apply_query(Conn, Name)],
    case Tx of
        notx ->
            migerl_util:log_info("applying "++Name++" w/o tx!"),
            migerl_db:queries(Conn, Queries);
        _ ->
            migerl_util:log_info("applying "++Name++" with tx!"),
            migerl_db:tx_queries(Conn, Queries)
    end.
