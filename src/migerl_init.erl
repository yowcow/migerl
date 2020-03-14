-module(migerl_init).

-export([
         dispatch/2
        ]).

dispatch(Conn, Opts) ->
    create_table(Conn),
    create_dir(proplists:get_value(dir, Opts)).

create_table(Conn) ->
    migerl_db:create_table(Conn),
    migerl_util:log_info("done creating table for migration!").

create_dir(Dir) ->
    case file:make_dir(Dir) of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} ->
            migerl_util:log_error("failed creating dir "++ Dir, Reason)
    end,
    migerl_util:log_info("done creating directory for migration files!").
