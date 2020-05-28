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

datetime_test_() ->
    Cases = [
             {
              "from mysql timestamp",
              {{2019, 8, 3}, {13, 5, 59}},
              "2019-08-03 13:05:59"
             },
             {
              "from postgres timestamp",
              {{2019, 8, 3}, {13, 5, 59.987654321}},
              "2019-08-03 13:05:59"
             }
            ],
    F = fun({Name, Input, Expected}) ->
                Actual = migerl_util:datetime(Input),
                {Name, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).

list_files_test_() ->
    {setup,
     fun() ->
             {ok, Started} = application:ensure_all_started(migerl),
             Started
     end,
     fun(Apps) ->
             error_logger:tty(false),
             try [application:stop(App) || App <- Apps]
             after error_logger:tty(true)
             end
     end,
     fun(_) ->
             Cases = [
                      {
                       "w/ order file",
                       ?MYSQL_SCRIPT_DIR,
                       [
                        {"file1.sql", ?MYSQL_SCRIPT_DIR++"/file1.sql"},
                        {"file2.sql", ?MYSQL_SCRIPT_DIR++"/file2.sql"},
                        {"file4.sql", ?MYSQL_SCRIPT_DIR++"/file4.sql"},
                        {"file3.sql", ?MYSQL_SCRIPT_DIR++"/file3.sql"}
                       ]
                      },
                      {
                       "w/ order file",
                       ?POSTGRES_SCRIPT_DIR,
                       [
                        {"file1.sql", ?POSTGRES_SCRIPT_DIR++"/file1.sql"},
                        {"file2.sql", ?POSTGRES_SCRIPT_DIR++"/file2.sql"},
                        {"file3.sql", ?POSTGRES_SCRIPT_DIR++"/file3.sql"}
                       ]
                      }
                     ],
             F = fun({Name, Opts, Expected}) ->
                         Actual = migerl_util:list_files(Opts),
                         {Name, ?_assertEqual(Expected, Actual)}
                 end,
             lists:map(F, Cases)
     end
    }.

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
