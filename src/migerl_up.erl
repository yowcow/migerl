-module(migerl_up).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    All = proplists:get_value(all, Opts),
    run_migrations(Conn, Files, All).

run_migrations(_, [], _) -> ok;
run_migrations(Conn, [{Name, Path} | Rem], false) ->
    case migerl_db:is_applied(Conn, Name) of
        false ->
            apply_one(Conn, Name, migerl_util:read_file(Path));
        _ ->
            run_migrations(Conn, Rem, false)
    end;
run_migrations(Conn, [{Name, Path} | Rem], true) ->
    case migerl_db:is_applied(Conn, Name) of
        false ->
            apply_one(Conn, Name, migerl_util:read_file(Path));
        _ ->
            ok
    end,
    run_migrations(Conn, Rem, true).

apply_one(Conn, Name, Content) ->
    {Tx, Queries0} = migerl_util:read_up(Content),
    Queries = [{Q , []} || Q <- Queries0] ++ [migerl_db:apply_query(Conn, Name)],
    case Tx of
        notx -> migerl_db:queries(Conn, Queries);
        _    -> migerl_db:tx_queries(Conn, Queries)
    end.
