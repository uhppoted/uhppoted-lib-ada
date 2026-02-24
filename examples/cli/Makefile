CLI = ./bin/cli

clean:
	alr clean

build:
	alr build

find-controllers: build
	$(CLI) find-controllers

get-controller: build
# 	$(CLI) get-controller --controller 405419896
	$(CLI) get-controller --controller 405419896 --address 192.168.1.125:60000 --protocol udp
