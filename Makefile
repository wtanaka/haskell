all: lint test ProblemThirtyEight

# Profile with ./ProgramName +RTS -p
%: %.hs
	ghc -prof -fprof-auto -rtsopts $^

.PHONY: lint
lint:
	hlint .

.PHONY: test
test:
	./testsuite/runtests.sh

clean:
	rm -f *~ ProblemThirtyEight *.o *.prof *.hi
