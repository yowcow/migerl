-module(migerl_sql).

-export([
    parse/1
]).

-spec parse(list()) -> list(list()).
parse(SQL) ->
    {ok, Tokens, _} = erl_scan:string(SQL, 1, [text, return_white_spaces]),
    build_queries(Tokens, false, [], []).

build_queries([], _, Cur, Acc) ->
    case Cur of
        [] ->
            lists:reverse(Acc);
        _ ->
            lists:reverse([build_query(Cur) | Acc])
    end;
% '--' comment begins
build_queries([{'--', _} | T], false, Cur, Acc) ->
    build_queries(T, true, Cur, Acc);
% '--' comment ends
build_queries([{white_space, _, [$\n | _]} | T], true, Cur, Acc) ->
    build_queries(T, false, Cur, Acc);
% skip everything while in comment
build_queries([_ | T], true, Cur, Acc) ->
    build_queries(T, true, Cur, Acc);
% ignore whitespaces
build_queries([{white_space, _, _} | T], false, Cur, Acc) ->
    build_queries(T, false, Cur, Acc);
% end of a query
build_queries([{';', _} | T], false, Cur, Acc) ->
    case Cur of
        [] ->
            build_queries(T, false, [], Acc);
        _ ->
            build_queries(T, false, [], [build_query(Cur) | Acc])
    end;
% otherwise append to current query
build_queries([{_, Prop} | T], false, Cur, Acc) ->
    build_queries(T, false, [proplists:get_value(text, Prop) | Cur], Acc);
build_queries([{_, Prop, _} | T], false, Cur, Acc) ->
    build_queries(T, false, [proplists:get_value(text, Prop) | Cur], Acc).

build_query(Words) ->
    string:join(lists:reverse(Words), " ").
