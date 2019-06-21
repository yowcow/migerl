-module(migerl_status).

-export([
    dispatch/2
]).

dispatch(Conn, Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Files = migerl_util:list_dir(Dir),
    Len = max_length([Name || {Name, _} <- Files], 4),
    show_migrations(Conn, Files, Len),
    ok.

show_migrations(_, [], _) -> ok;
show_migrations(Conn, [{Name, _} | Rem], Len) ->
    io:format("| ~ts | ~ts |~n", [string:left(Name, Len), string:left("Not applied", 20)]),
    show_migrations(Conn, Rem, Len).

max_length([], Max) -> Max;
max_length([Item | Rem], Max) ->
    Len = length(Item),
    case Len > Max of
        true -> max_length(Rem, Len);
        _    -> max_length(Rem, Max)
    end.
