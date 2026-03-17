CLI = ./bin/cli

CONTROLLER ?= 405419896
DEST ?= ""
PROTOCOL ?= default

ADDRESS ?= 192.168.1.125
NETMASK ?= 255.255.255.0
GATEWAY ?= 192.168.1.1

LISTENER ?= 192.168.1.125:60001
INTERVAL ?= 0

clean:
	alr clean

build:
	alr build

find-controllers: build
	$(CLI) find-controllers

get-controller: build
	$(CLI) get-controller --controller "$(CONTROLLER)" --dest $(DEST) --protocol $(PROTOCOL)

set-IPv4: build
	$(CLI) set-IPv4 --controller "$(CONTROLLER)" --address "$(ADDRESS)" --netmask "$(NETMASK)" --gateway "$(GATEWAY)"

get-time: build
	$(CLI) get-time --controller "$(CONTROLLER)"

set-time: build
	$(CLI) set-time --controller "$(CONTROLLER)"

get-listener: build
	$(CLI) get-listener --controller "$(CONTROLLER)"

set-listener: build
	$(CLI) set-listener --controller "$(CONTROLLER)" --dest "$(DEST)" --protocol "$(PROTOCOL)" --listener "$(LISTENER)" --interval "$(INTERVAL)"

get-status: build
	$(CLI) get-status --controller "$(CONTROLLER)"

