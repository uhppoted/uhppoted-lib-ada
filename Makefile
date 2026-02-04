clean:
	cd lib && make clean

build:
	cd lib && make build

find-controllers: build
	cd examples/cli && make find-controllers

