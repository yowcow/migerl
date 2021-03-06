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
              "--select 0 \n"
              "select 1, -- this is one 	 \n "
              "  	2      -- this is two 	 \r\n "
              ";  	-- select 3 -- this is three	 \n "
              ";  	--select 4 -- this is four\n ",
              ["select 1, 2"]
             },
             {
              "with backquotes",
              "select `val_1`, \n `val_2`;",
              ["select `val_1`, `val_2`"]
             },
             {
              "with nested parenthesis",
              "select unix_timestamp(now()) ; ",
              [
               "select unix_timestamp(now())"
              ]
             },
             {
              "type declaration",
              "int(10) unsigned ; ",
              [
               "int(10) unsigned"
              ]
             },
             {
              "where clause with `in`",
              "where name in ((hoge),(fuga),(foo))  ;  ",
              [
               "where name in((hoge), (fuga), (foo))"
              ]
             },
             {
              "key definitions",
              "PRIMARY KEY (`id`), UNIQUE KEY `foo_uniq` (`foo`)) ENGINE = InnoDB",
              [
               "PRIMARY KEY(`id`), UNIQUE KEY `foo_uniq`(`foo`)) ENGINE = InnoDB"
              ]
             }
            ],
    F = fun({Name, Input, Expected}) ->
                {Name,
                 fun() ->
                         Actual = migerl_sql:parse(Input),
                         ?assertEqual(Expected, Actual)
                 end
                }
        end,
    lists:map(F, Cases).
