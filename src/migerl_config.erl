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
    {ok, Device} = file:open(File, [read]),
    {ok, Data} = io:read(Device, ''),
    file:close(Device),
    proplists:get_value(Env, Data).
