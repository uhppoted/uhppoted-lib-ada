CLI = ./bin/cli

CONTROLLER ?= 405419896
PROTOCOL ?= udp

clean:
	alr clean

build:
	alr build

find-controllers: build
	$(CLI) find-controllers

get-controller: build
# 	$(CLI) get-controller --controller "$(CONTROLLER)"
	$(CLI) get-controller --controller "$(CONTROLLER, 192.168.1.125:60000, $(PROTOCOL)"

set-IPv4: build
	$(CLI) set-IPv4 --controller "$(CONTROLLER)" --address 192.168.1.125 --netmask 255.255.255.0 --gateway 192.168.1.1
