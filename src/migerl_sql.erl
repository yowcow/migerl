-module(migerl_sql).

-export([
         parse/1
        ]).

-spec parse(list()) -> list(list()).
parse(SQL) ->
    {ok, Tokens, _} = migerl_lexer:string(SQL),
    [build_query(Query) || Query <- build_queries(Tokens), length(Query) > 0].

build_queries(Tokens) ->
    build_queries(Tokens, [], []).

build_queries([], [], Acc) ->
    lists:reverse(Acc);
build_queries([], Cur, Acc) ->
    build_queries([], [], [lists:reverse(Cur)|Acc]);
build_queries([";"|T], Cur, Acc) ->
    build_queries(T, [], [lists:reverse(Cur)|Acc]);
build_queries([begin_comment|T0], Cur, Acc) ->
    {T, _} = gather_until(end_comment, T0, []),
    build_queries(T, Cur, Acc);
build_queries([begin_mysql_comment|T0], Cur0, Acc) ->
    {T, Cur} = gather_until(end_comment, T0, Cur0),
    build_queries(T, Cur, Acc);
build_queries([Token|T], Cur, Acc) ->
    build_queries(T, [Token|Cur], Acc).

gather_until(_, [], Acc) ->
    {[], Acc};
gather_until(Expr, [Expr|T], Acc) ->
    {T, Acc};
gather_until(Expr, [Token|T], Acc) ->
    gather_until(Expr, T, [Token|Acc]).

-define(BREAKS, [",", "(", ")", ";", begin_comment, begin_mysql_comment]).

build_query(Words) ->
    build_query(Words, []).

build_query([Token], Acc) ->
    lists:flatten(lists:reverse([Token|Acc]));
build_query([Token0, Next|T], Acc) ->
    BeforeBreak = lists:member(Next, ?BREAKS),
    Token = if
                Token0 =:= "(" -> Token0;
                Token0 =:= "," -> Token0++" ";
                BeforeBreak    -> Token0;
                true           -> Token0++" "
            end,
    build_query([Next|T], [Token|Acc]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

gather_until_test_() ->
    Cases = [
             {
              "nothing to gather",
              end_comment,
              [], [foo, bar],
              {[], [foo, bar]}
             },
             {
              "gather and acc",
              end_comment,
              [a, b, c, end_comment, d, e, f],
              [foo, bar],
              {
               [d, e, f],
               [c, b, a, foo, bar]
              }
             }
            ],
    F = fun({Title, Expr, Tokens, Acc, Expected}) ->
                Actual = gather_until(Expr, Tokens, Acc),
                {Title, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).

build_queries_test_() ->
    Cases = [
             {
              "multiple queries",
              [
               "select", "1", ";",
               "select", "2", ";"
              ],
              [
               ["select", "1"],
               ["select", "2"]
              ]
             },
             {
              "queries with comment",
              [
               "select", "1", ";",
               "select", begin_comment, "this", "is", "a", "comment", end_comment, "0", ";",
               "select", "2", ";"
              ],
              [
               ["select", "1"],
               ["select", "0"],
               ["select", "2"]
              ]
             },
             {
              "queries with mysqldump comment",
              [
               "select", "1", ";",
               begin_mysql_comment, "SET", "NAMES", "utf8", end_comment, ";",
               "select", "2", ";"
              ],
              [
               ["select", "1"],
               ["SET", "NAMES", "utf8"],
               ["select", "2"]
              ]
             }
            ],
    F = fun({Title, Tokens, Expected}) ->
                Actual = build_queries(Tokens),
                {Title, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).
-endif.
