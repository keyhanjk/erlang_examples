# erlang_examples
Collection of source codes which demonstrate all features of ERLANG 

#### Sample
```erlang
-module(hello_world).
-export([hello/0]).

hello() ->
    io:format("hello world~n").
```

#### Compile by Make

```console
$ make module=hello_world init=hello
compiling ...
running ...
hello world
```

#### Compile by Erlang

##### Compile the erlang program
```console
$ erlc helloworld.erl
```

##### Execute the erlang Program
```console
$ erl -noshell -s helloworld start -s init stop
```

##### Erlang one liner
```console
$ erl -noshell -eval 'io:fwrite("Hello, World!\n"), init:stop().'
```
