-module(migerl_util_tests).

-include_lib("eunit/include/eunit.hrl").

timestamp_test_() ->
    Cases = [
        {
            "returns timestamp string",
            {1560,660962,772976},
            "20190616045602"
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = migerl_util:timestamp(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

list_dir_test_() ->
    Cases = [
        {
            "returns a sorted list of files",
            "test/files",
            [
                {"file1.sql", "test/files/file1.sql"},
                {"file2.sql", "test/files/file2.sql"},
                {"file3.sql", "test/files/file3.sql"}
            ]
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = migerl_util:list_dir(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

read_up_test_() ->
    Cases = [
        {
            "no query when empty",
            "",
            []
        },
        {
            "no query when not defined",
            "\n\n"
            "-- +migrate Up\n"
            "\n\n"
            "-- +migrate Down\n",
            []
        },
        {
            "no query when no up or down section is defined",
            "hogehoge\n"
            "fugafuga\n",
            []
        },
        {
            "returns 1 up query",
            "\n"
            "-- +migrate Up\n"
            "hoge1\n"
            "hoge2\n"
            "fuga1\n"
            "fuga2\n",
            ["hoge1\nhoge2\nfuga1\nfuga2\n"]
        },
        {
            "returns multiple up queries",
            "\n"
            "-- +migrate Up\n"
            "hoge1\n"
            "hoge2;\n"
            "\n"
            "fuga1\n"
            "fuga2\n",
            [
                "hoge1\nhoge2;",
                "fuga1\nfuga2\n"
            ]
        },
        {
            "returns 1 up query when down section is defined",
            "\n"
            "-- +migrate Up\n"
            "hoge1\n"
            "hoge2\n"
            "fuga1\n"
            "fuga2\n"
            "\n"
            "-- +migrate Down\n"
            "foo1\n"
            "bar1\n"
            "foo2\n"
            "bar2\n",
            ["hoge1\nhoge2\nfuga1\nfuga2\n"]
        },
        {
            "returns 1 up query when down section is defined before up section",
            "\n"
            "-- +migrate Down\n"
            "foo1\n"
            "bar1\n"
            "foo2\n"
            "bar2\n"
            "-- +migrate Up\n"
            "hoge1\n"
            "hoge2\n"
            "fuga1\n"
            "fuga2\n",
            ["hoge1\nhoge2\nfuga1\nfuga2\n"]
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = migerl_util:read_up(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).

read_down_test_() ->
    Cases = [
        {
            "no query when empty",
            "",
            []
        },
        {
            "no query when not defined",
            "\n\n"
            "-- +migrate Up\n"
            "\n\n"
            "-- +migrate Down\n",
            []
        },
        {
            "no query when no up or down section is defined",
            "hogehoge\n"
            "fugafuga\n",
            []
        },
        {
            "returns 1 down query",
            "\n"
            "-- +migrate Down\n"
            "hoge1\n"
            "hoge2\n"
            "fuga1\n"
            "fuga2\n",
            ["hoge1\nhoge2\nfuga1\nfuga2\n"]
        },
        {
            "returns multiple down queries",
            "\n"
            "-- +migrate Up\n"
            "foo1\n"
            "bar1\n"
            "foo2\n"
            "bar2\n"
            "\n"
            "-- +migrate Down\n"
            "hoge1\n"
            "hoge2;\n"
            "\n"
            "fuga1\n"
            "fuga2\n"
            "\n",
            ["hoge1\nhoge2;", "fuga1\nfuga2\n"]
        },
        {
            "returns a down query when up section is defined after down section",
            "\n"
            "-- +migrate Down\n"
            "hoge1\n"
            "hoge2\n"
            "fuga1\n"
            "fuga2\n"
            "\n"
            "-- +migrate Up\n"
            "foo1\n"
            "bar1\n"
            "foo2\n"
            "bar2\n",
            ["hoge1\nhoge2\nfuga1\nfuga2\n"]
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = migerl_util:read_down(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).
