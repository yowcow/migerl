-module(migerl_status).
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
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    Results = migerl_db:get_status(Conn, Files),
    migerl_util:log_info("----- current migration status -----"),
    show_status(Results).

show_status([]) -> ok;
show_status([{Name, _, wont_be_applied} | Rem]) ->
    Msg = io_lib:format("[~ts]   ... skipped ...   <- `~ts`", [color:redb(?UTF8_CROSS), Name]),
    migerl_util:log_info(Msg),
    show_status(Rem);
show_status([{Name, _, will_be_applied} | Rem]) ->
    Msg = io_lib:format("[~ts]   ... pending ...   <- `~ts`", [color:blueb(?UTF8_CIRCL), Name]),
    migerl_util:log_info(Msg),
    show_status(Rem);
show_status([{Name, _, Timestamp} | Rem]) ->
    Msg = io_lib:format("[~ts] ~ts <- `~ts`", [color:greenb(?UTF8_CHECK), migerl_util:datetime(Timestamp), Name]),
    migerl_util:log_info(Msg),
    show_status(Rem).
