module 		:= hello_world
init 		:= hello

CC      	:= erlc
RUNNER		:= erl
PROGRAM 	:= $(module)
FUNCTION 	:= $(init)
CCFLAGS 	:= -noshell -s $(PROGRAM) $(FUNCTION) -s init stop

all: run

compile: 
	$(info compiling ...)
	@$(RUNNER) -make
	@$(CC) $(PROGRAM).erl
	
run: compile
	$(info running ...)
	@$(RUNNER) $(CCFLAGS)

clean:
	$(info cleaning up ...)
	@rm -f *.beam