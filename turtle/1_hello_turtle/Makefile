TARGET=hello-turtle

all:
	ghc ${TARGET}.hs -o h
	gcc hello-c.c -o c

.Phony: clean
clean:
	rm -f *.o *.hi c h
