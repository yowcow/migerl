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
              "'#' style comments are removed",
              "# hoge\n"
              "#	hoge\n"
              "select 1; #fuga\n",
              [
               "select 1"
              ]
             },
             {
              "'--' style comments are removed",
              "--select 0\n;"
              "select 1--1, -- this is one 	 \n "
              "  	1+2      -- this is two 	 \r\n "
              ";  	-- select 3 -- this is three	 \n "
              ";  	--	select 4 -- this is four\n ",
              [
               "- - select 0",
               "select 1 - -1, 1 +2"
              ]
             },
             {
              "/* */ style comments are removed",
              "select 1 /* hoge \n"
              "fuga \n*/"
              "; select 2 /* \n"
              "select 3",
              [
               "select 1",
               "select 2"
              ]
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
             },
             {
              "mysqldump",
              "--\n"
              "-- Dumping data for table `hogehoge`\n"
              "--\n"
              "\n"
              "LOCK TABLES `fugafuga` WRITE;\n"
              "/*!40000 ALTER TABLE `fugafuga` DISABLE KEYS */;\n"
              "/*!40000 ALTER TABLE `fugafuga` ENABLE KEYS */;\n"
              "UNLOCK TABLES;\n"
              "ENGINE=InnoDB \n"
              "/*!50100 PARTITION BY RANGE (TO_DAYS(`date`))\n"
              "(PARTITION p1 VALUES LESS THAN (100) ENGINE = InnoDB,\n"
              " PARTITION p2 VALUES LESS THAN (200) ENGINE = InnoDB,\n"
              " PARTITION p3 VALUES LESS THAN (300) ENGINE = InnoDB) */;",
              [
               "LOCK TABLES `fugafuga` WRITE",
               "ALTER TABLE `fugafuga` DISABLE KEYS",
               "ALTER TABLE `fugafuga` ENABLE KEYS",
               "UNLOCK TABLES",
               "ENGINE = InnoDB PARTITION BY RANGE(TO_DAYS(`date`))"
               "(PARTITION p1 VALUES LESS THAN(100) ENGINE = InnoDB, "
               "PARTITION p2 VALUES LESS THAN(200) ENGINE = InnoDB, "
               "PARTITION p3 VALUES LESS THAN(300) ENGINE = InnoDB)"
              ]
             }
            ],
    F = fun({Name, Input, Expected}) ->
                Actual = migerl_sql:parse(Input),
                {Name, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).
