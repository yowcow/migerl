-module(migerl_init).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    create_table(Conn),
    create_dir(proplists:get_value(dir, Opts)).

create_table({Dialect, _} = Conn) ->
    {Query, Args} = create_query(Dialect),
    migerl_db:query(Conn, Query, Args),
    migerl_util:log_info("done creating table for migration!").

create_query(mysql) ->
    Query = "CREATE TABLE IF NOT EXISTS `migrations` ("
           "`id` varchar(254) NOT NULL, "
           "`applied_at` datetime DEFAULT NULL, "
           "PRIMARY KEY (`id`) "
           ") ENGINE=InnoDB DEFAULT CHARSET=utf8",
    {Query, []}.

create_dir(Dir) ->
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} ->
            migerl_util:log_error("failed creating dir "++ Dir, Reason)
    end,
    migerl_util:log_info("done creating directory for migration files!").
