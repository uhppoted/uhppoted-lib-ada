clean:
	cd lib && make clean

build:
	cd lib && make build

get-controllers: build
	cd examples/cli && make get-controllers

