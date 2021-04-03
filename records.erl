-module(records).

-export([show/0]).

-record(person, {name, age, status = single}).

show() ->
    P1 = #person{name = "Joe Doe", age = "25"},
    io:format("Created person ~p~n", [P1#person.name]),
    io:format("Record fields: ~p~n",
              [record_info(fields, person)]),
    io:format("Record size: ~p~n",
              [record_info(size, person)]).
