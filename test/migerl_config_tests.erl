-module(migerl_config_tests).

-include_lib("eunit/include/eunit.hrl").

load_test_() ->
    Cases = [
        {
            "returns config under default",
            [
                {config, "test/valid.config"},
                {env, "default"}
            ],
            [
                {dialect, mysql},
                {config, [
                    {host, "127.0.0.1"},
                    {port, 3306},
                    {user, "root"},
                    {password, "migerl"},
                    {database, "migerl_test"},
                    {log_warnings, false},
                    {queries, [
                        "SET NAMES utf8"
                    ]}
                ]}
            ]
        },
        {
            "returns config under devel",
            [
                {config, "test/valid.config"},
                {env, "devel"}
            ],
            [
                {dialect, mysql},
                {config, [
                    {host, "127.0.0.1"},
                    {port, 3306},
                    {user, "devel-user"},
                    {password, "migerl"},
                    {database, "migerl_devel"},
                    {log_warnings, false},
                    {queries, [
                        "SET NAMES utf8"
                    ]}
                ]}
            ]
        }
    ],
    F = fun({Name, Input, Expected}) ->
        Actual = migerl_config:load(Input),
        {Name, ?_assertEqual(Expected, Actual)}
    end,
    lists:map(F, Cases).
