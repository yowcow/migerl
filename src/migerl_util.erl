-module(migerl_util).

-export([
    log_error/2,
    log_info/1,
    log_info/2,
    timestamp/1,
    datetime/1,
    list_dir/1,
    read_file/1,
    read_up/1,
    read_down/1
]).

log_error(Msg, Args) ->
    io:format("~n~ts: ~p~n~n", [Msg, Args]),
    error(Args).

log_info(Msg) ->
    io:format("-> ~ts~n", [Msg]).

log_info(Msg, Args) ->
    io:format("~ts: ~p~n", [Msg, Args]).

timestamp(Timestamp) ->
    Second = posix_second(Timestamp),
    {{Y, M, D}, {Hour, Min, Sec}} = calendar:system_time_to_universal_time(Second, second),
    io_lib:format("~4.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B", [Y, M, D, Hour, Min, Sec]).

posix_second({MegaSec, Sec, _}) ->
    MegaSec * 1000000 + Sec.

datetime({{Y, Mo, D}, {H, M, S}}) ->
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, Mo, D, H, M, S]).

list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            [{File, Dir++"/"++File} || File <- lists:sort(Files)];
        Err ->
            log_error("failed opening directory '"++Dir++"'", Err)
    end.

read_file(Filepath) ->
    {ok, Bin} = file:read_file(Filepath),
    binary_to_list(Bin).

read_up(Data) ->
    read_queries("Up", string:split(Data, "\n", all)).

read_down(Data) ->
    read_queries("Down", string:split(Data, "\n", all)).

read_queries(_, []) -> undefined;
read_queries(Mark, [Line | Rem]) ->
    case re:run(Line, "^-- \\+migrate "++ Mark++"([\\s\\t]+notransaction)?") of
        {match, [_]} ->
            read_queries(tx, Rem, []);
        {match, [_, _]} ->
            read_queries(notx, Rem, []);
        _ ->
            read_queries(Mark, Rem)
    end.

read_queries(_, [], []) -> undefined;
read_queries(Tx, [], Acc) ->
    SQL = lists:flatten(lists:reverse(Acc)),
    {Tx, migerl_sql:parse(SQL)};
read_queries(Tx, [[] | L], Acc) ->
    read_queries(Tx, L, Acc);
read_queries(Tx, [Line | L], Acc) ->
    case re:run(Line, "^-- \\+migrate .+") of
        {match, _} ->
            read_queries(Tx, [], Acc);
        _ ->
            read_queries(Tx, L, [Line++"\n" | Acc])
    end.
