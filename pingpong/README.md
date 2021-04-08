concurrent-distributed-examples
===============================

A collection of examples written in Erlang that illustrates concurrent and distributed systems.

- PingPong
- Messenger

### PingPong

In order to run it in the same machine, open two different terminals and do the following:

**Terminal 1**

```
$ erl -sname pong		# Start a node with name 'pong'
$ c(pingpong). 			# Compile the pingpong module
$ pingpong:start_pong().	# Start pong :)
```

**Terminal 2**

```
$ erl -sname ping           			# Start a node with name 'ping'
$ c(pingpong).                   		# Compile the pingpong module
$ pingpong:start_ping('pong@gvolpe-HXT4').      # Start ping with location of the pong node, that in my case, is the name of my machine 'gvolpe-HXT4'.
```

### Messenger

TODO