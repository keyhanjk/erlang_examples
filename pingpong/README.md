
### PingPong

** Connect Nodes **

```
erl -sname node1 -setcookie sample_cookie
erl -sname node2 -setcookie sample_cookie
```

you can just copy $HOME/.erlang.cookie to other remote pc for sharing same cookie value.


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

**Sample**

```
pingpong:start_pong().
pingpong:start_ping('node2@ibm').
```
