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
        [] ->
            lists:reverse(Acc);
        _ ->
            lists:reverse([build_query(Cur) | Acc])
    end;
build_queries([";" | T], Cur, Acc) ->
    case Cur of
        [] ->
            build_queries(T, [], Acc);
        _ ->
            build_queries(T, [], [build_query(Cur) | Acc])
    end;
build_queries(["," | T], Cur, Acc) ->
    case Cur of
        [] ->
            build_queries(T, [","], Acc);
        [CurH | CurT] ->
            build_queries(T, [CurH++"," | CurT], Acc)
    end;
build_queries(["(" | T], Cur, Acc) ->
    case Cur of
        [] ->
            build_queries(T, ["("], Acc);
        [CurH | CurT] ->
            build_queries(T, [CurH++"(" | CurT], Acc)
    end;
build_queries([")" | T], Cur, Acc) ->
    case Cur of
        [] ->
            build_queries(T, [")"], Acc);
        [CurH | CurT] ->
            build_queries(T, [CurH++")" | CurT], Acc)
    end;
build_queries([Word | T], Cur, Acc) ->
    build_queries(T, [Word | Cur], Acc).

build_query(Words) ->
    string:join(lists:reverse(Words), " ").
