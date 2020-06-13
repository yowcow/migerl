-module(migerl_lexer_tests).

-include_lib("eunit/include/eunit.hrl").

string_test_() ->
    Cases = [
             {
              "bare words",
              "hoge, fuga ,foo234 bar_345",
              ["hoge", ",", "fuga", ",", "foo234", "bar_345"]
             },
             {
              "strings",
              "\"hoge\" hoge; 'fuga' fuga; `foo` foo;",
              ["\"hoge\"", "hoge", ";", "'fuga'", "fuga", ";", "`foo`", "foo", ";"]
             },
             {
              "numbers",
              "123 1.23 -123 -1.23 .123 -.123 1.23E008 -1.23e008 .123e008 -.123E008",
              ["123", "1.23", "-123", "-1.23", ".123", "-.123", "1.23E008", "-1.23e008", ".123e008", "-.123E008"]
             },
             {
              "arithmetic",
              "1+2-3*4/5=hoge--1*fuga/foo+bar\n"
              "! !! !!!",
              [
               "1", "+2", "-3", "*4", "/5", "=", "hoge", "-", "-1", "*", "fuga", "/", "foo", "+", "bar",
               "!", "!!", "!!!"
              ]
             },
             {
              "comment block",
              "--\n"
              "-- hoge hoge\n"
              "--	fuga fuga\n"
              "--\n"
              "hoge1 fuga1--1\n"
              "hoge2 fuga2 -- bar\n"
              "/*this is \na comment*/",
              [
               "hoge1", "fuga1", "-", "-1", "hoge2", "fuga2",
               begin_comment, "this", "is", "a", "comment", end_comment
              ]
             },
             {
              "mysqldump comment",
              "/*!40101 SET NAMES utf8 */;",
              [
               begin_comment,
               "!", "40101", "SET", "NAMES", "utf8",
               end_comment,
               ";"
              ]
             },
             {
              "operators",
              "x=1\n"
              "x!=1\n"
              "x<>1\n"
              "x>1\n"
              "x>=1\n"
              "x<1\n"
              "x<=1\n",
              [
               "x", "=", "1",
               "x", "!=", "1",
               "x", "<>", "1",
               "x", ">", "1",
               "x", ">=", "1",
               "x", "<", "1",
               "x", "<=", "1"
              ]
             },
             {
              "realworld sql",
              "/*!40101 SET NAMES utf8\n"
              "hogehoge*/;\n"
              "select `hoge` as `HOGE` --this aint a comment -- this is a comment\n"
              "from ( \n"
              "  select hoge from `fuga` /*fuga table*/ \n"
              "  where name=\"--foo--\" "
              ") subq\n",
              [
               begin_comment,
               "!", "40101", "SET", "NAMES", "utf8", "hogehoge",
               end_comment, ";",
               "select", "`hoge`", "as", "`HOGE`", "-", "-", "this", "aint", "a", "comment",
               "from", "(",
               "select", "hoge", "from", "`fuga`",
               begin_comment, "fuga", "table", end_comment,
               "where", "name", "=", "\"--foo--\"",
               ")", "subq"
              ]
             }
            ],
    F = fun({Name, Input, Expected}) ->
                {ok, Tokens, _} = migerl_lexer:string(Input),
                {Name, ?_assertEqual(Expected, Tokens)}
        end,
    lists:map(F, Cases).
