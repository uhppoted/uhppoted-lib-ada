CONTROLLER ?= 405419896
DEST ?= ""
PROTOCOL ?= default

LISTENER  ?= 192.168.1.125:60001
INTERVAL  ?= 0
DOOR      ?= 3
MODE      ?= controlled
DELAY     ?= 5
PASSCODES ?= 7531,12345,54321

CARD       ?= 10058400
INDEX      ?= 3
START_DATE ?= 2026-01-01
END_DATE   ?= 2026-12-31
DOORS      ?= 1,3,4
PIN        ?= 7531

clean:
	alr clean

build:
	alr build

test: build
	cd tests && make test

find-controllers: build
	cd ../examples/cli && make find-controllers

get-controller: build
	cd ../examples/cli && make get-controller

set-IPv4: build
	cd ../examples/cli && make set-IPv4

get-time: build
	cd ../examples/cli && make get-time

set-time: build
	cd ../examples/cli && make set-time

get-listener: build
	cd ../examples/cli && make get-listener

set-listener: build
	cd ../examples/cli && make set-listener CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) LISTENER=$(LISTENER) INTERVAL=$(INTERVAL)

get-status: build
	cd ../examples/cli && make get-status

get-door: build
	cd ../examples/cli && make get-door CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) DOOR=$(DOOR)

set-door: build
	cd ../examples/cli && make set-door CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) DOOR=$(DOOR) MODE="$(MODE)" DELAY=$(DELAY)

set-door-passcodes: build
	cd ../examples/cli && make set-door-passcodes CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) DOOR=$(DOOR) PASSCODES=$(PASSCODES)

open-door: build
	cd ../examples/cli && make open-door CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) DOOR=$(DOOR)

get-cards: build
	cd ../examples/cli && make get-cards CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL)

get-card: build
	cd ../examples/cli && make get-card CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) CARD=$(CARD)

get-card-at-index: build
	cd ../examples/cli && make get-card-at-index CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) INDEX=$(INDEX)

put-card: build
	cd ../examples/cli && make put-card CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) CARD=$(CARD) START=$(START_DATE) END=$(END_DATE) ACCESS=$(DOORS) PIN=$(PIN)

delete-card: build
	cd ../examples/cli && make delete-card CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL) CARD=$(CARD)

delete-all-cards: build
	cd ../examples/cli && make delete-all-cards CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL)

get-event-index: build
	cd ../examples/cli && make get-event-index CONTROLLER=$(CONTROLLER) DEST=$(DEST) PROTOCOL=$(PROTOCOL)
