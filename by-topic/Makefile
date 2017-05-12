SUB = types

.PHONY: 4
4:
	ghc -o out 4_infix.hs

.PHONY: clean
clean:
	rm -f *.hi *.o out
	for dir in $(SUB); do (cd $$dir && ${MAKE} clean); done		
