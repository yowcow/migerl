-module(migerl_init).
-behavior(migerl_dispatcher_behavior).
-export([dispatch/1]).

-ifdef(TEST).
-export([dispatch/2]).
-endif.

-include("config.hrl").

-spec dispatch([migerl:option()]) -> term().
dispatch(Opts) ->
    Config = migerl_config:load(Opts),
    Conn = migerl_db:start(Config),
    dispatch(Conn, Opts).

dispatch(Conn, Opts) ->
    create_table(Conn),
    create_dir(proplists:get_value(dir, Opts)).

create_table(Conn) ->
    migerl_db:create_table(Conn),
    migerl_util:log_info("done creating table for migration!").

create_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            %% create execution order file
            OrderFile = Dir++"/"++?ORDER_FILE,
            ok = file:write_file(OrderFile, [template()]),
            ok;
        {error, eexist} -> ok;
        {error, Reason} ->
            migerl_util:log_error("failed creating dir "++ Dir, Reason)
    end,
    migerl_util:log_info("done creating directory for migration files!").

template() ->
    "---\n".
