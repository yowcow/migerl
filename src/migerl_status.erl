-module(migerl_status).

-export([
    dispatch/2
]).

-include("config.hrl").

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    show_migrations(Conn, Files),
    ok.

show_migrations(_, []) -> ok;
show_migrations(Conn, [{Name, _} | Rem]) ->
    case migerl_db:get_applied_at(Conn, Name) of
        undefined ->
            io:format("[ ] '~s' is NOT applied.~n", [Name]);
        {{Y, Mo, D}, {H, M, S}} ->
            io:format(
                "[~ts] '~s' was APPLIED at "
                "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B!~n",
                [
                    ?UTF8_CHECK, Name,
                    Y, Mo, D, H, M, S
                ]
            )
    end,
    show_migrations(Conn, Rem).
