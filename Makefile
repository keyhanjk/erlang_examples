module 		:= hello_world
call 		:= hello

CC      	:= erlc
RUNNER		:= erl
PROGRAM 	:= $(module)
FUNCTION 	:= $(call)
CCFLAGS 	:= -noshell -s $(PROGRAM) $(FUNCTION) -s init stop

all: run

compile: 
	$(info compiling ...)
	@$(CC) $(PROGRAM).erl
	
run: compile
	$(info running ...)
	@$(RUNNER) $(CCFLAGS)

.PHONY: clean

clean:
	$(info cleaning up ...)
	@rm -f *.beam *.dump




