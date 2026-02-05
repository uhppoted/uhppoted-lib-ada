clean:
	cd lib && make clean

build:
	cd lib && make build

test:
	cd lib && make test

find-controllers: build
	cd examples/cli && make find-controllers

