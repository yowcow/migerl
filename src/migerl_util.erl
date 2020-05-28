-module(migerl_util).

-export([
         log_error/2,
         log_info/1,
         log_info/2,
         log_queries/1,
         timestamp/1,
         datetime/1,
         list_files/1,
         read_file/1,
         read_up/1,
         read_down/1
        ]).

-include("config.hrl").

log_error(Msg, Args) ->
    io:format("~n~ts: ~p~n~n", [Msg, Args]),
    error(Args).

log_info(Msg) ->
    io:format("~ts~n", [Msg]).

log_info(Msg, Args) ->
    io:format("~ts: ~p~n", [Msg, Args]).

log_queries([]) -> ok;
log_queries([Q | Queries]) ->
    io:format("~ts~n~n", [Q]),
    log_queries(Queries).

timestamp(Timestamp) ->
    Second = posix_second(Timestamp),
    {{Y, M, D}, {Hour, Min, Sec}} = calendar:system_time_to_universal_time(Second, second),
    io_lib:format("~4.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B", [Y, M, D, Hour, Min, Sec]).

posix_second({MegaSec, Sec, _}) ->
    MegaSec * 1000000 + Sec.

datetime({Date, {H, M, S}}) when is_float(S) ->
    datetime({Date, {H, M, trunc(S)}});
datetime({{Y, Mo, D}, {H, M, S}}) ->
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, Mo, D, H, M, S]).

list_files(Dir) ->
    OrderFile = Dir++"/"++?ORDER_FILE,
    case filelib:is_regular(OrderFile) of
        true ->
            list_order(Dir, OrderFile);
        _ ->
            list_dir(Dir)
    end.

list_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            [
             {File, Dir++"/"++File}
             || File <- lists:sort(Files),
                lists:suffix(".sql", File)
            ];
        Err ->
            log_error("failed opening directory '"++Dir++"'", Err)
    end.

list_order(Dir, OrderFile) ->
    [Files] = try yamerl_constr:file(OrderFile)
              catch
                  _:Err -> throw({invalid_yaml, Err})
              end,
    [
     {File, Dir++"/"++File}
     || File <- Files
    ].

read_file(Filepath) ->
    {ok, Bin} = file:read_file(Filepath),
    binary_to_list(Bin).

read_up(Data) ->
    find_queries("Up", string:split(Data, "\n", all)).

read_down(Data) ->
    find_queries("Down", string:split(Data, "\n", all)).

find_queries(_, []) -> undefined;
find_queries(Mark, [Line | Rem]) ->
    case re:run(Line, "^-- \\+migrate "++ Mark) of
        {match, _} ->
            read_queries(Rem, []);
        _ ->
            find_queries(Mark, Rem)
    end.

read_queries([], []) -> undefined;
read_queries([], Acc) ->
    SQL = lists:flatten(lists:reverse(Acc)),
    parse_queries(migerl_sql:parse(SQL));
read_queries([[] | L], Acc) ->
    read_queries(L, Acc);
read_queries([Row | L], Acc) ->
    case re:run(Row, "^-- \\+migrate .+") of
        {match, _} ->
            read_queries([], Acc);
        _ ->
            read_queries(L, [Row++"\n" | Acc])
    end.

-define(TX_STATEMENTS, ["INSERT", "UPDATE", "DELETE", "SELECT"]).

parse_queries(Queries) ->
    parse_queries(Queries, Queries, true).

parse_queries([], Queries, true) ->
    {tx, Queries};
parse_queries([], Queries, false) ->
    {notx, Queries};
parse_queries([Q | L], Queries, Tx) ->
    Stmt = extract_statement(Q),
    case lists:member(Stmt, ?TX_STATEMENTS) of
        true ->
            parse_queries(L, Queries, Tx and true);
        _ ->
            parse_queries(L, Queries, Tx and false)
    end.

extract_statement(Q) ->
    [Stmt | _] = string:split(Q, " "),
    string:uppercase(Stmt).
