DIRS = library executable test-suite benchmark
EXE = wavetoy1

all: indent lint build test coverage bench doc exec

setup:
#	TODO: Remove MacPorts (/opt/local) from PATH
	stack setup
	stack install hlint

indent:
#	find $(DIRS) -name '*.hs' -print0 | xargs -0 -P4 -n1 hindent
	find $(DIRS) -name '*.hs' -print0 | \
		xargs -0 -P4 -n1 ./write-if-changed.sh brittany

lint: indent
	hlint lint --report --no-exit-code $(DIRS)
build: indent
	stack build
test: indent
#	stack test --coverage
	stack test
coverage: indent test
#	stack hpc report
bench: indent
	stack bench
doc: indent
	stack haddock
exec: indent
	stack exec $(EXE)

clean:
	stack clean
	rm -f $(EXE).cabal
	rm -f report.html

.PHONY: all setup indent lint build test coverage bench doc exec clean
