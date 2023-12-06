all: compile run gen_bytecode clean run_java 

compile:
	ghc main.hs

run:
	./main

clear_console:
	clear

gen_bytecode:
	java -jar jasmin/jasmin.jar Wallace.k7

run_java:
	java Wallace

clean:
	rm *.o
	rm *.hi