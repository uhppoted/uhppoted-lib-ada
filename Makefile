SRC = lib/src/*.ads lib/src/*.adb

CONTROLLER ?= 405419896
DEST ?= ""
TRANSPORT ?= default

LISTENER  ?= 192.168.1.125:60001
INTERVAL  ?= 0
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
TIME_PROFILE_ID ?= 29
TIME_PROFILE ?= 2026-01-01,2026-12-31,[Mon,Tues,Fri],[08:30-16:45,19:15-22:00],17
TASK ?= unlock door,2026-01-01,2026-12-31,[Mon,Tues,Fri],08:30,3,13
PC_CONTROL ?= yes
INTERLOCK ?= 1&2,3&4
KEYPADS ?= 1,2,4
ANTIPASSBACK ?= (1,3):(2,4)
FIRSTCARD ?= 08:30,16:45,normally open,firstcard only,[Mon,Tues,Fri]

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
	cd examples/cli && make build
	cd integration-tests && make build

adadoc: $(SRC)
	mkdir -p adadoc
	cd lib && alr exec -- gnatdoc -l --warnings  --style leading -P uhppoted_lib_ada.gpr -O ../adadoc 2>&1 | grep -Ei "warning|error" | wc -l
	cd lib && alr exec -- gnatdoc -l --warnings  --style leading -P uhppoted_lib_ada.gpr -O ../adadoc

adadoc-watch: $(SRC)
	find ./lib/src -name "*.ad[sb]" | entr -c make adadoc

debug:
	cd examples/cli && make get-controller CONTROLLER=405419896 DEST=192.168.1.125:60000 TRANSPORT=udp

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
	cd examples/cli && make set-listener CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) LISTENER=$(LISTENER) INTERVAL=$(INTERVAL)

get-status: build
	cd examples/cli && make get-status

get-door: build
	cd examples/cli && make get-door CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) DOOR=$(DOOR)

set-door: build
	cd examples/cli && make set-door CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) DOOR=$(DOOR) MODE="$(MODE)" DELAY=$(DELAY)

set-door-passcodes: build
	cd examples/cli && make set-door-passcodes CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) DOOR=$(DOOR) PASSCODES=$(PASSCODES)

open-door: build
	cd examples/cli && make open-door CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) DOOR=$(DOOR)

get-cards: build
	cd examples/cli && make get-cards CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT)

get-card: build
	cd examples/cli && make get-card CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) CARD=$(CARD)

get-card-at-index: build
	cd examples/cli && make get-card-at-index CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) CARD_INDEX=$(CARD_INDEX)

put-card: build
	cd examples/cli && make put-card CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) CARD=$(CARD) START=$(START_DATE) END=$(END_DATE) ACCESS=$(DOORS) PIN=$(PIN)

delete-card: build
	cd examples/cli && make delete-card CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) CARD=$(CARD)

delete-all-cards: build
	cd examples/cli && make delete-all-cards CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT)

get-event: build
	cd examples/cli && make get-event CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) EVENT_INDEX=$(EVENT_INDEX)

get-event-index: build
	cd examples/cli && make get-event-index CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT)

set-event-index: build
	cd examples/cli && make set-event-index CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) EVENT_INDEX=$(EVENT_INDEX)

record-special-events: build
	cd examples/cli && make record-special-events CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) RECORD_SPECIAL_EVENTS=$(RECORD_SPECIAL_EVENTS)

get-time-profile: build
	cd examples/cli && make get-time-profile CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) TIME_PROFILE_ID=$(TIME_PROFILE_ID) 

set-time-profile: build
	cd examples/cli && make set-time-profile CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) TIME_PROFILE_ID=$(TIME_PROFILE_ID) TIME_PROFILE="$(TIME_PROFILE)"

clear-time-profiles: build
	cd examples/cli && make clear-time-profiles CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT)

add-task: build
	cd examples/cli && make add-task CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) TASK="$(TASK)"

refresh-tasklist: build
	cd examples/cli && make refresh-tasklist CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT)

clear-tasklist: build
	cd examples/cli && make clear-tasklist CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT)

set-pc-control: build
	cd examples/cli && make set-pc-control CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) PC_CONTROL=$(PC_CONTROL)

set-interlock: build
	cd examples/cli && make set-interlock CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) INTERLOCK="$(INTERLOCK)"

activate-keypads: build
	cd examples/cli && make activate-keypads CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) KEYPADS="$(KEYPADS)"

get-antipassback: build
	cd examples/cli && make get-antipassback CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT)

set-antipassback: build
	cd examples/cli && make set-antipassback CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) ANTIPASSBACK="$(ANTIPASSBACK)"

set-firstcard: build
	cd examples/cli && make set-firstcard CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT) DOOR=$(DOOR) FIRSTCARD="$(FIRSTCARD)"

restore-default-parameters: build
	cd examples/cli && make restore-default-parameters CONTROLLER=$(CONTROLLER) DEST=$(DEST) TRANSPORT=$(TRANSPORT)

listen: build
	cd examples/cli && make listen
