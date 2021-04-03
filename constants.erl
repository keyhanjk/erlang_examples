-module(constants).

-export([show/0]).

-define(N, 123).

-define(M, "what").

-define(SQUARED(X), X * X).

show() ->
    io:format("N = ~p ~n", [?N]),
    io:format("M = ~p ~n", [?M]),
    io:format("~p ~n", [?SQUARED(5)]).
