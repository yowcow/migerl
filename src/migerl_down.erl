-module(migerl_down).
-behavior(migerl_dispatcher_behavior).
-export([dispatch/1]).

-ifdef(TEST).
-export([dispatch/2]).
-endif.

-spec dispatch([migerl:option()]) -> term().
dispatch(Opts) ->
    Config = migerl_config:load(Opts),
    Conn = migerl_db:start(Config),
    dispatch(Conn, Opts).

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    Migrations = migerl_db:get_status(Conn, Files),
    All = proplists:get_value(all, Opts),
    unapply_migrations(Conn, lists:reverse(Migrations), All).

unapply_migrations(_, [], _) ->
    migerl_util:log_info("-> no more migrations to unapply!"),
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
    migerl_util:log_info("-> unapplying "++Name++" but it has no query to run!"),
    {Query, Args} = migerl_db:unapply_query(Conn, Name),
    migerl_db:query(Conn, Query, Args);
unapply_one(Conn, Name, {Tx, Queries}) ->
    BoundQueries = [{Q , []} || Q <- Queries] ++ [migerl_db:unapply_query(Conn, Name)],
    case Tx of
        notx ->
            migerl_util:log_info("-> unapplying "++Name++" without tx..."),
            migerl_util:log_queries(Queries),
            migerl_db:queries(Conn, BoundQueries);
        _ ->
            migerl_util:log_info("-> unapplying "++Name++" with tx..."),
            migerl_util:log_queries(Queries),
            migerl_db:tx_queries(Conn, BoundQueries)
    end.
