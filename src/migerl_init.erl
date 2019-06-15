-module(migerl_init).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    create_table(Conn),
    create_dir(proplists:get_value(dir, Opts)).

create_table({Dialect, _} = Conn) ->
    Query = create_query(Dialect),
    migerl_db:query(Conn, Query).

create_query(mysql) ->
    Stmt = "CREATE TABLE IF NOT EXISTS `migrations` ("
           "`id` varchar(254) NOT NULL, "
           "`applied_at` datetime DEFAULT NULL, "
           "PRIMARY KEY (`id`) "
           ") ENGINE=InnoDB DEFAULT CHARSET=utf8",
    {Stmt, []}.

create_dir(Dir) ->
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} ->
            error("failed creating dir " ++ Dir ++ ": " ++ atom_to_list(Reason))
    end.
