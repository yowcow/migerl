-module(migerl_down).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    All = proplists:get_value(all, Opts),
    unapply_migrations(Conn, lists:reverse(Files), All).

unapply_migrations(_, [], _) -> ok;
unapply_migrations(Conn, [{Name, Path} | Rem], false) ->
    case migerl_db:is_applied(Conn, Name) of
        true ->
            unapply_one(Conn, Name, Path);
        _ ->
            unapply_migrations(Conn, Rem, false)
    end;
unapply_migrations(Conn, [{Name, Path} | Rem], true) ->
    case migerl_db:is_applied(Conn, Name) of
        true ->
            unapply_one(Conn, Name, Path),
            unapply_migrations(Conn, Rem, true);
        _ ->
            unapply_migrations(Conn, Rem, true)
    end.

unapply_one(Conn, Name, Path) ->
    Content = migerl_util:read_file(Path),
    {Tx, Queries0} = migerl_util:read_down(Content),
    Queries = [{Q , []} || Q <- Queries0] ++ [migerl_db:unapply_query(Conn, Name)],
    case Tx of
        notx -> migerl_db:queries(Conn, Queries);
        _    -> migerl_db:tx_queries(Conn, Queries)
    end.
