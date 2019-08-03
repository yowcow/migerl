-module(migerl_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include("testing.hrl").

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
            ?MYSQL_SCRIPT_DIR,
            [
                {"file1.sql", ?MYSQL_SCRIPT_DIR++"/file1.sql"},
                {"file2.sql", ?MYSQL_SCRIPT_DIR++"/file2.sql"},
                {"file3.sql", ?MYSQL_SCRIPT_DIR++"/file3.sql"}
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
            undefined
        },
        {
            "no query when not defined",
            "\n\n"
            "-- +migrate Up\n"
            "\n\n"
            "-- +migrate Down\n",
            undefined
        },
        {
            "no query when no up or down section is defined",
            "hogehoge\n"
            "fugafuga\n",
            undefined
        },
        {
            "returns 1 up query",
            "\n"
            "-- +migrate Up\n"
            "hoge1\n"
            "hoge2\n"
            "fuga1\n"
            "fuga2\n",
            {notx, ["hoge1 hoge2 fuga1 fuga2"]}
        },
        {
            "returns multiple up queries",
            "\n"
            "-- +migrate Up    notransaction\n"
            "hoge1\n"
            "hoge2 ; \n"
            "\n"
            "fuga1\n"
            "fuga2\n",
            {notx, [
                "hoge1 hoge2",
                "fuga1 fuga2"
            ]}
        },
        {
            "returns 1 up query when down section is defined",
            "\n"
            "-- +migrate Up notransaction\n"
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
            {notx, ["hoge1 hoge2 fuga1 fuga2"]}
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
            {notx, ["hoge1 hoge2 fuga1 fuga2"]}
        },
        {
            "returns transactionable queries",
            "-- +migrate Up notransaction\n"
            "insert into hoge;"
            "update hoge set hoge=\"hoge\";"
            "delete hoge;"
            "select * from hoge;\n\n",
            {tx, [
                "insert into hoge",
                "update hoge set hoge = \"hoge\"",
                "delete hoge",
                "select * from hoge"
            ]}
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
            undefined
        },
        {
            "no query when not defined",
            "\n\n"
            "-- +migrate Up\n"
            "\n\n"
            "-- +migrate Down\n",
            undefined
        },
        {
            "no query when no up or down section is defined",
            "hogehoge\n"
            "fugafuga\n",
            undefined
        },
        {
            "returns 1 down query",
            "\n"
            "-- +migrate Down\n"
            "hoge1\n"
            "hoge2\n"
            "fuga1\n"
            "fuga2\n",
            {notx, ["hoge1 hoge2 fuga1 fuga2"]}
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
            "-- +migrate Down  notransaction\n"
            "hoge1\n"
            "hoge2 ;;; \n"
            "\n"
            "fuga1\n"
            "fuga2\n"
            "\n",
            {notx, ["hoge1 hoge2", "fuga1 fuga2"]}
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
            {notx, ["hoge1 hoge2 fuga1 fuga2"]}
        },
        {
            "returns transactionable queries",
            "-- +migrate Down notransaction\n"
            "insert into hoge;"
            "update hoge set hoge=\"hoge\";"
            "delete hoge;"
            "select * from hoge;\n\n",
            {tx, [
                "insert into hoge",
                "update hoge set hoge = \"hoge\"",
                "delete hoge",
                "select * from hoge"
            ]}
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = migerl_util:read_down(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).
