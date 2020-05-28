-module(migerl_new_tests).

-include_lib("eunit/include/eunit.hrl").
-include("config.hrl").

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
                {_, Actual} = migerl_new:filepath(Dir, Title, Timestamp),
                {Name, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).

dispatch_test_() ->
    {setup,
     fun() ->
             Dir = "/tmp/migerl-new",
             os:cmd("rm -rf "++Dir),
             file:make_dir(Dir),
             OrderFile = Dir++"/"++?ORDER_FILE,
             ok = file:write_file(OrderFile, ["---\n"]),
             Dir
     end,
     fun(_Dir) ->
             ok
     end,
     fun(Dir) ->
             {_, Filepath} = migerl_new:dispatch([{dir, Dir}, {title, "hoge fuga"}]),
             {ok, Bin} = file:read_file(Filepath),
             Actual = binary_to_list(Bin),
             Expected = "-- +migrate Up\n\n-- +migrate Down\n\n",
             [
              ?_assertEqual(Expected, Actual)
             ]
     end
    }.
