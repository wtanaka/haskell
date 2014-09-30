GHCFLAGS=-W -Werror
BINARIES=Cat Wc \
	ProblemThirtyEight.debug

all: lint $(BINARIES) test

# Profile with ./ProgramName +RTS -h -p; hp2ps ProgName.hp
%.debug: %.hs
	ghc $(GHCFLAGS) -prof -fprof-auto -rtsopts $^ -o "$@"

%: %.hs
	ghc $(GHCFLAGS) -O2 $^ && strip $@

%.debug.ps: %.debug
	rm -f "$<".hp ; ./"$<" +RTS -h; hp2ps "$<".hp

.PHONY: lint
lint:
	hlint .

.PHONY: test
test: $(BINARIES)
	./testsuite/runtests.sh

clean:
	rm -f *~ $(BINARIES) *.o *.prof *.hi *.hp *.ps *.aux
