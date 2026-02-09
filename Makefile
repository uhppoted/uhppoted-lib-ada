clean:
	cd lib && make clean

build:
	cd lib && make build

test:
	cd lib && make test

integration-tests: build
	cd integration-tests && make test

build-all:
	cd lib && alr --non-interactive update
	cd lib && alr --non-interactive build

find-controllers: build
	cd examples/cli && make find-controllers

