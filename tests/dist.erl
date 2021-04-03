-module(t).

-export([t/1]).

t(From) -> From ! node().
