-module(migerl_config).

-export([
    load/1,
    load/2
]).

load(Opts) ->
    Env = proplists:get_value(env, Opts),
    File = proplists:get_value(config, Opts),
    load(Env, File).

load(Env, File) ->
    read(Env, file:open(File, [read])).

read(Env, {ok, Device}) ->
    {ok, Data} = io:read(Device, ''),
    file:close(Device),
    proplists:get_value(Env, Data);
read(_, Error) ->
    migerl_util:log_error("failed opening config", Error).
