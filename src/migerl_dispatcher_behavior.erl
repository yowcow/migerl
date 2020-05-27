-module(migerl_dispatcher_behavior).

-callback dispatch([migerl:option()]) -> term().

