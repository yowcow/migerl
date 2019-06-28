-module(migerl_status).

-export([
    dispatch/2
]).

-include("config.hrl").

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    Results = migerl_db:get_status(Conn, Files),
    migerl_util:log_info("--- current status ---"),
    show_status(Results).

show_status([]) -> ok;
show_status([{Name, _, wont_be_applied} | Rem]) ->
    Msg = io_lib:format("~ts '~ts` WON'T BE applied!", [?UTF8_CROSS, Name]),
    migerl_util:log_info(Msg),
    show_status(Rem);
show_status([{Name, _, will_be_applied} | Rem]) ->
    Msg = io_lib:format("~ts '~ts` WILL BE applied!", [?UTF8_CIRCL, Name]),
    migerl_util:log_info(Msg),
    show_status(Rem);
show_status([{Name, _, Timestamp} | Rem]) ->
    Msg = io_lib:format("~ts '~ts` WAS applied at ~ts!", [?UTF8_CHECK, Name, migerl_util:datetime(Timestamp)]),
    migerl_util:log_info(Msg),
    show_status(Rem).
