-module(migerl_sql).

-export([
    parse/1
]).

-spec parse(list()) -> list(list()).
parse(SQL) ->
    {ok, Tokens, _} = migerl_lexer:string(SQL),
    build_queries(Tokens, [], []).

build_queries([], Cur, Acc) ->
    case Cur of
        [] -> lists:reverse(Acc);
        _  -> lists:reverse([build_query(Cur) | Acc])
    end;
build_queries([";"|T], Cur, Acc) ->
    case Cur of
        [] -> build_queries(T, [], Acc);
        _  -> build_queries(T, [], [build_query(Cur) | Acc])
    end;
build_queries([Tok], Cur, Acc) ->
    build_queries([], [Tok|Cur], Acc);
build_queries([Tok, Next|T], Cur, Acc) ->
    BeforeBreak = lists:member(Next, [",", "(", ")", ";"]),
    Tok1 = if
        Tok =:= "(" -> Tok;
        Tok =:= "," -> Tok++" ";
        BeforeBreak -> Tok;
        true        -> Tok++" "
    end,
    build_queries([Next|T], [Tok1|Cur], Acc).

build_query(Words) ->
    lists:flatten(lists:reverse(Words)).
