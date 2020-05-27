-module(migerl_new_tests).

-include_lib("eunit/include/eunit.hrl").

filepath_test_() ->
    Cases = [
             {
              "creates a filename",
              {
               "path/to",
               "hoge fuga foo bar",
               {1560,594217,305419}
              },
              "path/to/20190615102337-hoge_fuga_foo_bar.sql"
             },
             {
              "filename is sanitized",
              {
               "path/to",
               " ~!?/hoge ~!?/fuga ~!?/",
               {1560,594217,305419}
              },
              "path/to/20190615102337-_____hoge_____fuga_____.sql"
             }
            ],
    F = fun({Name, {Dir, Title, Timestamp}, Expected}) ->
                Actual = migerl_new:filepath(Dir, Title, Timestamp),
                {Name, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).

dispatch_test() ->
    File = migerl_new:dispatch([{dir, "/tmp"}, {title, "hoge fuga"}]),
    {ok, Bin} = file:read_file(File),
    Actual = binary_to_list(Bin),
    Expected = "-- +migrate Up\n\n-- +migrate Down\n\n",
    ?assertEqual(Expected, Actual).
