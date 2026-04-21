CLI = ./bin/cli

CONTROLLER ?= 405419896
DEST ?= ""
PROTOCOL ?= default

ADDRESS ?= 192.168.1.125
NETMASK ?= 255.255.255.0
GATEWAY ?= 192.168.1.1

LISTENER ?= 192.168.1.125:60001
INTERVAL ?= 0

DOOR      ?= 3
MODE      ?= controlled
DELAY     ?= 5
PASSCODES ?= 7531,12345,54321

CARD       ?= 10058400
CARD_INDEX ?= 3
START_DATE ?= 2026-01-01
END_DATE   ?= 2026-12-31
DOORS      ?= 1,3,4
PIN        ?= 7531

EVENT_INDEX ?= 23
RECORD_SPECIAL_EVENTS ?= yes

clean:
	alr clean

build:
	alr build

find-controllers: build
	$(CLI) find-controllers

get-controller: build
	$(CLI) get-controller --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL)

set-IPv4: build
	$(CLI) set-IPv4 --controller $(CONTROLLER) --address $(ADDRESS) --netmask $(NETMASK) --gateway $(GATEWAY)

get-time: build
	$(CLI) get-time --controller $(CONTROLLER)

set-time: build
	$(CLI) set-time --controller $(CONTROLLER)

get-listener: build
	$(CLI) get-listener --controller $(CONTROLLER)

set-listener: build
	$(CLI) set-listener --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --listener $(LISTENER) --interval $(INTERVAL)

get-status: build
	$(CLI) get-status --controller $(CONTROLLER)

get-door: build
	$(CLI) get-door --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --door $(DOOR)

set-door: build
	$(CLI) set-door --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --door $(DOOR) --mode $(MODE) --delay $(DELAY)

set-door-passcodes: build
	$(CLI) set-door-passcodes --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --door $(DOOR) --passcodes $(PASSCODES)

open-door: build
	$(CLI) open-door --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --door $(DOOR)

get-cards: build
	$(CLI) get-cards --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL)

get-card: build
	$(CLI) get-card --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --card $(CARD)

get-card-at-index: build
	$(CLI) get-card-at-index --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --index $(CARD_INDEX)

put-card: build
	$(CLI) put-card --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --card $(CARD) --start $(START_DATE) --end $(END_DATE) --doors $(DOORS) --PIN $(PIN)

delete-card: build
	$(CLI) delete-card --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --card $(CARD)

delete-all-cards: build
	$(CLI) delete-all-cards --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL)

get-event-index: build
	$(CLI) get-event-index --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL)

set-event-index: build
	$(CLI) set-event-index --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --index $(EVENT_INDEX)

record-special-events: build
	$(CLI) record-special-events --controller $(CONTROLLER) --dest $(DEST) --protocol $(PROTOCOL) --enabled $(RECORD_SPECIAL_EVENTS)
