clean:
	cd lib && make clean

build:
	cd lib && make build

test:
	cd lib && make test

build-all: clean build

find-controllers: build
	cd examples/cli && make find-controllers

