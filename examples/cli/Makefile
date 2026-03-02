CLI = ./bin/cli

CONTROLLER ?= 405419896
UDP ?= 405419896, 192.168.1.125:60000, udp
TCP ?= 405419896, 192.168.1.125:60000, tcp

clean:
	alr clean

build:
	alr build

find-controllers: build
	$(CLI) find-controllers

get-controller: build
	$(CLI) get-controller --controller "$(CONTROLLER)"
	$(CLI) get-controller --controller "$(CONTROLLER), $(ADDRESS), $(PROTOCOL)"

set-IPv4: build
	$(CLI) set-IPv4 --controller "$(CONTROLLER)" --address 192.168.1.125 --netmask 255.255.255.0 --gateway 192.168.1.1

get-time: build
	$(CLI) get-time --controller "$(CONTROLLER)"
