CONTROLLER ?= 405419896
DEST ?= ""
PROTOCOL ?= default

LISTENER ?= 192.168.1.125:60001
INTERVAL ?= 0
DOOR     ?= 3

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

get-time: build
	cd examples/cli && make get-time

set-time: build
	cd examples/cli && make set-time

get-listener: build
	cd examples/cli && make get-listener

set-listener: build
	cd examples/cli && make set-listener CONTROLLER="$(CONTROLLER)" DEST="$(DEST)" PROTOCOL="$(PROTOCOL)" LISTENER="$(LISTENER)" INTERVAL="$(INTERVAL)"

get-status: build
	cd examples/cli && make get-status

get-door: build
	cd examples/cli && make get-door CONTROLLER="$(CONTROLLER)" DEST="$(DEST)" PROTOCOL="$(PROTOCOL)" DOOR="$(DOOR)"
