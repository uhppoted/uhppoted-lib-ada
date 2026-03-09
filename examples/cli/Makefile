CLI = ./bin/cli

CONTROLLER ?= 405419896
# CONTROLLER ?= 405419896,192.168.1.125:60000,udp

ADDRESS ?= 192.168.1.125
NETMASK ?= 255.255.255.0
GATEWAY ?= 192.168.1.1

clean:
	alr clean

build:
	alr build

find-controllers: build
	$(CLI) find-controllers

get-controller: build
	$(CLI) get-controller --controller "$(CONTROLLER)"

set-IPv4: build
	$(CLI) set-IPv4 --controller "$(CONTROLLER)" --address "$(ADDRESS)" --netmask "$(NETMASK)" --gateway "$(GATEWAY)"

get-time: build
	$(CLI) get-time --controller "$(CONTROLLER)"

set-time: build
	$(CLI) set-time --controller "$(CONTROLLER)"

get-status: build
	$(CLI) get-status --controller "$(CONTROLLER)"

