-module(migerl).

-export([
    main/1
]).

-include("config.hrl").

-define(VERSION, [0, 0, 7]).

%% escript Entry point
main(Args) ->
    {ok, {Opts, Commands}} = getopt:parse(?OPT_SPEC, Args),
    Help = proplists:get_value(help, Opts),
    Ver = proplists:get_value(version, Opts),
    if
        Help ->
            getopt:usage(
                ?OPT_SPEC, "migerl", "[command ...]",
                [{"command", "Commands to execute (e.g. init, new, status, up, down)"}]
            );
        Ver ->
            io:format("migerl ~.10B.~.10B.~.10B~n", ?VERSION);
        true ->
            Config = migerl_config:load(Opts),
            Conn = migerl_db:start(Config),
            dispatch(Conn, Opts, Commands)
    end,
    erlang:halt(0).

dispatch(Conn, _, []) ->
    migerl_db:stop(Conn),
    done;
dispatch(Conn, Opts, [Comm | Rem]) ->
    case Comm of
        "init" ->
            migerl_init:dispatch(Conn, Opts);
        "new" ->
            migerl_new:dispatch(Conn, Opts);
        "up" ->
            migerl_up:dispatch(Conn, Opts);
        "down" ->
            migerl_down:dispatch(Conn, Opts);
        "status" ->
            migerl_status:dispatch(Conn, Opts);
        _ ->
            migerl_util:log_error("unknown command", Comm)
    end,
    dispatch(Conn, Opts, Rem).
