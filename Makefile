all: compile run clean

compile:
	ghc main.hs

run:
	./main


clean:
	rm *.o
	rm *.hi
	rm main