SUB = types

.PHONY: clean
clean:
	rm -f *.hi *.o
	for dir in $(SUB); do (cd $$dir && ${MAKE} clean); done		
