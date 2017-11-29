EXE = wavetoy1

all:
	stack -j4 setup
	hlint lint -j4 --report --no-exit-code library executable test-suite benchmark
	stack -j4 build
	stack test
	stack bench
	stack -j4 haddock
	stack exec $(EXE)

clean:
	rm -f report.html
	stack clean
