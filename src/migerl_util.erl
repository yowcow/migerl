-module(migerl_util).

-export([
    log_error/2,
    log_info/1,
    log_info/2,
    timestamp/1,
    list_dir/1,
    read_file/1,
    read_up/1,
    read_down/1
]).

log_error(Msg, Args) ->
    io:format("~n~ts: ~p~n~n", [Msg, Args]),
    error(Args).

log_info(Msg) ->
    io:format("~ts~n", [Msg]).

log_info(Msg, Args) ->
    io:format("~ts: ~p~n", [Msg, Args]).

timestamp(Timestamp) ->
    Second = posix_second(Timestamp),
    {{Y, M, D}, {Hour, Min, Sec}} = calendar:system_time_to_universal_time(Second, second),
    io_lib:format("~4.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B", [Y, M, D, Hour, Min, Sec]).

posix_second({MegaSec, Sec, _}) ->
    MegaSec * 1000000 + Sec.

list_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    [{File, Dir++"/"++File} || File <- lists:sort(Files)].

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
            read_queries(tx, Rem, [], []);
        {match, [_, _]} ->
            read_queries(notx, Rem, [], []);
        _ ->
            read_queries(Mark, Rem)
    end.

read_queries(_, [], [], []) -> undefined;
read_queries(Tx, [], [], Acc) ->
    {Tx, lists:reverse(Acc)};
read_queries(Tx, [], Current, Acc) ->
    read_queries(Tx, [], [], [lists:flatten(lists:reverse(Current)) | Acc]);

read_queries(Tx, [[] | Rem], Current, Acc) ->
    read_queries(Tx, Rem, Current, Acc);
read_queries(Tx, [Line | Rem], Current, Acc) ->
    case re:run(Line, "^-- \\+migrate .+") of
        {match, _} ->
            read_queries(Tx, [], Current, Acc);
        _ ->
            case re:run(Line, ";$") of
                {match, _} ->
                    read_queries(Tx, Rem, [], [lists:flatten(lists:reverse([Line | Current])) | Acc]);
                _ ->
                    read_queries(Tx, Rem, [Line++"\n" | Current], Acc)
            end
    end.
