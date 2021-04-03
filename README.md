# erlang_examples
Collection of source codes which demonstrate all features of ERLANG 

#### Compile by Make

```console
$ make compile
$ make run
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
