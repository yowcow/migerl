-module(migerl_lexer_tests).

-include_lib("eunit/include/eunit.hrl").

string_test_() ->
    Cases = [
             {
              "bare words",
              "hoge, fuga ,234",
              ["hoge", ",", "fuga", ",", "234"]
             },
             {
              "strings",
              "\"hoge\" hoge; 'fuga' fuga; `foo` foo;",
              ["\"hoge\"", "hoge", ";", "'fuga'", "fuga", ";", "`foo`", "foo", ";"]
             },
             {
              "comment block",
              "hoge1 fuga1 --foo \n"
              "hoge2 fuga2 -- bar\n",
              ["hoge1", "fuga1", "hoge2", "fuga2"]
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
              "arithmetic",
              "1+2-3*4/5=hoge",
              ["1", "+", "2", "-", "3", "*", "4", "/", "5", "=", "hoge"]
             },
             {
              "realworld sql",
              "select `hoge` as `HOGE`--this is comment \n"
              "from ( \n"
              "  select hoge from `fuga` /*fuga table*/ \n"
              "  where name=\"--foo--\" "
              ") subq\n",
              [
               "select", "`hoge`", "as", "`HOGE`",
               "from", "(",
               "select", "hoge", "from", "`fuga`", "/*", "fuga", "table", "*/",
               "where", "name", "=", "\"--foo--\"",
               ")", "subq"
              ]
             }
            ],
    F = fun({Name, Input, Expected}) ->
                {Name,
                 fun() ->
                         {ok, Tokens, _} = migerl_lexer:string(Input),
                         ?assertEqual(Expected, Tokens)
                 end
                }
        end,
    lists:map(F, Cases).
