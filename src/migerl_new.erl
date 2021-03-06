-module(migerl_new).
-behavior(migerl_dispatcher_behavior).
-export([dispatch/1]).

-ifdef(TEST).
-export([filepath/3]).
-endif.

-spec dispatch([migerl:option()]) -> term().
dispatch(Opts) ->
    create_script(
      proplists:get_value(dir, Opts),
      proplists:get_value(title, Opts)
     ).

create_script(Dir, Title) ->
    File = filepath(Dir, Title, os:timestamp()),
    case file:write_file(File, [template()]) of
        ok -> ok;
        {error, Reason} -> migerl_util:log_error("failed creating a script file", Reason)
    end,
    migerl_util:log_info("done creating a migration file!: "++File),
    File.

template() ->
    "-- +migrate Up\n\n"
    "-- +migrate Down\n\n".

filepath(Dir, Title, Timestamp) ->
    Prefix = migerl_util:timestamp(Timestamp),
    SafeTitle = sanitize_title(Title, ["/", " ", "?", "!", "~"]),
    lists:flatten(io_lib:format("~s/~s-~s.sql", [Dir, Prefix, SafeTitle])).

sanitize_title(Title, []) -> Title;
sanitize_title(Title0, [Pattern | Rem]) ->
    Title = string:replace(Title0, Pattern, "_", all),
    sanitize_title(Title, Rem).
