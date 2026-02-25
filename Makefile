clean:
	cd lib && make clean

generate:
	cd .codegen && make generate

update:
	cd lib               && alr index --update-all
	cd lib/tests         && alr index --update-all
	cd integration-tests && alr index --update-all
	cd examples          && alr index --update-all

build:
	cd lib && make build

test:
	cd lib && make test

integration-tests: build
	cd integration-tests && make test

build-all:
	cd lib && alr --non-interactive update
	cd lib && make build

find-controllers: build
	cd examples/cli && make find-controllers

get-controller: build
	cd examples/cli && make get-controller

set-IPv4: build
	cd examples/cli && make set-IPv4
