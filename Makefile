all: compile clean clear_console run

compile:
	ghc main.hs

run:
	./main

clear_console:
	clear


clean:
	rm *.o
	rm *.hi