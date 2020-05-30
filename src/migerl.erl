-module(migerl).

-export([main/1]).

-include("config.hrl").

-define(VERSION, [0, 3, 0]).

-type option() :: getopt:option().
-type command() :: string().

-export_type([
              option/0,
              command/0
             ]).

-on_load(on_load/0).

on_load() ->
    application:start(yamerl).

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
            dispatch(Opts, Commands)
    end,
    erlang:halt(0).

-spec dispatch([option()], [command()]) -> ok.
dispatch(_, []) ->
    ok;
dispatch(Opts, [Command|T]) ->
    case Command of
        "init" ->
            migerl_init:dispatch(Opts);
        "new" ->
            migerl_new:dispatch(Opts);
        "up" ->
            migerl_up:dispatch(Opts);
        "down" ->
            migerl_down:dispatch(Opts);
        "status" ->
            migerl_status:dispatch(Opts);
        _ ->
            migerl_util:log_error("unknown command", Command)
    end,
    dispatch(Opts, T).
