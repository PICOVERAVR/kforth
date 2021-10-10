CC = gcc
SRC = $(filter-out test.S, $(wildcard *.S)) # grab all asm files except the test one
FLAGS = -no-pie # make this a standalone executable, no stdlib dependancies
DFLAGS = $(FLAGS) -g

UNAME := $(shell uname)

debug: pre
	@$(CC) $(DFLAGS) $(SRC)
	@echo "debug build completed."

release: pre
	@$(CC) $(FLAGS) $(SRC)-o release.out
	@tar -zcf KForth.tar.gz release.out #this is not accurate anymore.
	@echo "zipped build completed."

pre:
	@echo "building on: $(UNAME)"
	
clean:
	@rm -f	{*.out,*.o} #remove executable and any leftover object files
	@rm -f KForth.tar.gz

