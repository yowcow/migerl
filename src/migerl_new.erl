-module(migerl_new).

-export([
    dispatch/2,
    filepath/3
]).

dispatch(_, Opts) ->
    create_script(
        proplists:get_value(dir, Opts),
        proplists:get_value(title, Opts)
    ).

create_script(Dir, Title) ->
    File = filepath(Dir, Title, os:timestamp()),
    case file:write_file(File, [template()]) of
        ok -> ok;
        {error, Reason} -> error("failed creating a script file: " ++ atom_to_list(Reason))
    end,
    File.

template() ->
    "-- +migrate Up\n\n"
    "-- +migrate Down\n\n".

filepath(Dir, Title, Timestamp) ->
    Prefix = prefix(Timestamp),
    SafeTitle = sanitize_title(Title, ["/", " ", "?", "!", "~"]),
    lists:flatten(io_lib:format("~s/~s-~s.sql", [Dir, Prefix, SafeTitle])).

prefix(Timestamp) ->
    Second = posix_second(Timestamp),
    {{Y, M, D}, {Hour, Min, Sec}} = calendar:system_time_to_universal_time(Second, second),
    io_lib:format("~4.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B", [Y, M, D, Hour, Min, Sec]).

posix_second({MegaSec, Sec, _}) ->
    MegaSec * 1000000 + Sec.

sanitize_title(Title, []) -> Title;
sanitize_title(Title0, [Pattern | Rem]) ->
    Title = string:replace(Title0, Pattern, "_", all),
    sanitize_title(Title, Rem).
