GHCFLAGS=-W -Werror
BINARIES=Cat Wc ProblemThirtyEight.debug

all: lint $(BINARIES) test

# Profile with ./ProgramName +RTS -p
%.debug: %.hs
	ghc $(GHCFLAGS) -prof -fprof-auto -rtsopts $^ -o "$@"

%: %.hs
	ghc $(GHCFLAGS) -O2 $^ && strip $@

.PHONY: lint
lint:
	hlint .

.PHONY: test
test: $(BINARIES)
	./testsuite/runtests.sh

clean:
	rm -f *~ $(BINARIES) *.o *.prof *.hi
