GHCFLAGS=-W -Werror
BINARIES=Cat ProblemThirtyEight.debug

all: lint test $(BINARIES)

# Profile with ./ProgramName +RTS -p
%.debug: %.hs
	ghc $(GHCFLAGS) -prof -fprof-auto -rtsopts $^

%: %.hs
	ghc $(GHCFLAGS) -O2 $^ && strip $@

.PHONY: lint
lint:
	hlint .

.PHONY: test
test:
	./testsuite/runtests.sh

clean:
	rm -f *~ $(BINARIES) *.o *.prof *.hi
