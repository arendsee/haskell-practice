TARGET=example

all:
	ghc ${TARGET}.hs

.Phony: clean
clean:
	rm -f *.o *.hi ${TARGET} *.svg *.pdf

.Phony: render
render:
	./example -o z.svg -w 400
