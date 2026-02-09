CLI = ../examples/cli/bin/cli

clean:
	alr clean

build:
	alr build

test: build
	cd tests && make test

find-controllers: build
	cd ../examples/cli && make find-controllers


