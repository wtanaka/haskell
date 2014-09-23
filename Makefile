all: lint test

.PHONY: lint
lint:
	hlint .

.PHONY: test
test:
	./testsuite/runtests.sh

clean:
	rm -f *~
