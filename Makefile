SUB = 99 algorithms interval-tree monads by-topic io parsec smof turtle

# Recursively clean everything
.PHONY: clean
clean:
	for dir in $(SUB); do (cd $$dir && ${MAKE} clean); done
	rm -f TAGS tags
