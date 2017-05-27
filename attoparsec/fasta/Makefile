TARGET=Main

all:
	ghc ${TARGET}.hs

.Phony: clean
clean:
	rm -f *.o *.hi ${TARGET}

.Phony: run
run:
	./Main < foo.faa
