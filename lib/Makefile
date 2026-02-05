CLI = ../examples/cli/bin/cli

clean:
	alr clean

build:
	alr build

test: build
	alr exec -- gprbuild -P uhppoted_lib_ada_tests.gpr
	./bin/run_all_tests

find-controllers: build
	cd ../examples/cli && make find-controllers


