CC      	:= erlc
RUNNER		:= erl
SOURCE 		:= hello_world.erl
PROGRAM 	:= hello_world
FUNCTION 	:= hello
CCFLAGS 	:= erl -noshell -s $(PROGRAM) $(FUNCTION) -s init stop

compile: 
	$(CC) $(SOURCE) $(run)
	
run:
	$(RUNNER) $(CCFLAGS)

.PHONY: clean

clean:
	rm -f *.beam *.dump




