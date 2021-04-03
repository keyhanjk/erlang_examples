-module(gen_happy).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{get_happy, 0}, {get_ecstatic, 0}];
behaviour_info([]) -> ok;
behaviour_info(_) -> undefined.
