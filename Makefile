DIRS = library executable test-suite benchmark
EXE = wavetoy1

all: indent lint build test bench doc exec

setup:
	stack setup

indent:
#	find $(DIRS) -name '*.hs' -print0 | xargs -0 -P4 -n1 hindent
# 	find $(DIRS) -name '*.hs' -print0 | \
# 		xargs -0 -I '{}' -P4 -n1 brittany -i '{}' -o '{}'

lint: indent
	hlint lint --report --no-exit-code $(DIRS)
build: indent
	stack build
test: indent
	stack test
bench: indent
	stack bench
doc: indent
	stack haddock
exec: indent
	stack exec $(EXE)

clean:
	rm -f report.html
	stack clean

.PHONY: all setup indent lint build test bench doc exec clean
