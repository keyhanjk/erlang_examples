-module(hello_world).
-export([hello/0,stop/0]).

hello() ->
    io:format("hello world~n").

stop() ->
    io:format("hello world~n").