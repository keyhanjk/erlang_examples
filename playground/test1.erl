-module(test1).

-export([start/0]).

start() ->
    % io:fwrite("Hello, world!\n"),
    even([4, 6, 7, 3, 2]).

even([H | T]) when H rem 2 == 0 -> [H | even(T)];
even([_ | T]) ->
    io:fwrite("~p", T),
    even(T);
even([]) -> [].

what_is(Erlang) ->
    case Erlang of
        movie -> [hello(mike, joe, robert), credits];
        language -> formatting_arguments
    end.
