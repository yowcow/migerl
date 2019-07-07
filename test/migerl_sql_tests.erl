-module(migerl_sql_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Cases = [
        {
            "Empty queries",
            ";	;\n  ;\n  	\n;",
            []
        },
        {
            "Simple statement",
            "SELECT     1 ;   ;",
            ["SELECT 1"]
        },
        {
            "Multiple simple statement",
            "SELECT     1 	   	 ;	;  "
            "select 2 	, \";\"	   ",
            ["SELECT 1", "select 2, \";\""]
        },
        {
            "'--' style comments are removed",
            "select 1, -- this is one 	 \n "
            "  	2      -- this is two 	 \r\n "
            ";  	-- select 3 -- this is three	 \n ",
            ["select 1, 2"]
        },
        {
            "with backquotes",
            "select `val_1`, \n `val_2`;",
            ["select val_1, val_2"]
        },
        {
            "with parenthesis",
            "select unix_timestamp(now()) ; ",
            ["select unix_timestamp( now())"]
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = migerl_sql:parse(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).
