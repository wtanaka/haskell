GHCFLAGS=-W -Werror
BINARIES=Cat.out Wc.out \
	Mem1.debug.ps \
	Mem1.ps \
	Mem2.debug.ps \
	Mem2.ps \
	Mem3.debug.ps \
	Mem3.ps \
	MutualTailRecursion.ps \
	ProblemThirtyEight.debug.out

.PHONY: all
all: lint $(BINARIES) test

# Profile with ./ProgramName +RTS -h -p; hp2ps ProgName.hp
%.debug.out: %.hs
	ghc $(GHCFLAGS) -prof -fprof-auto -rtsopts $^ -o "$@"

%.out: %.hs
	ghc $(GHCFLAGS) -O2 -rtsopts $^ -o "$@" && strip $@

%.ps: %.out
	rm -f "$<".hp ; ./"$<" +RTS -h; hp2ps -c "$<".hp ; mv "$*".out.ps "$@"

%.prof: %.out
	./"$<" +RTS -p

.PHONY: lint
lint:
	hlint .

.PHONY: test
test: $(BINARIES)
	./testsuite/runtests.sh

clean:
	rm -f *~ $(BINARIES) *.o *.prof *.hi *.hp *.ps *.aux *.out
