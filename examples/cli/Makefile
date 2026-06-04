CLI = ./bin/cli

CONTROLLER ?= 405419896
DEST ?= ""
TRANSPORT ?= default

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
TIME_PROFILE_ID ?= 29
TIME_PROFILE ?= 2026-01-01,2026-12-31,[Mon,Tues,Fri],[08:30-16:45,19:15-22:00],17
TASK ?= unlock door,2026-01-01,2026-12-31,[Mon,Tues,Fri],08:30,3,13
PC_CONTROL ?= yes
INTERLOCK ?= 1&2,3&4
KEYPADS ?= 1,2,4
ANTIPASSBACK ?= (1,3):(2,4)
FIRSTCARD ?= 08:30,16:45,normally open,firstcard only,[Mon,Tues,Fri]

clean:
	alr clean

build:
	alr build

find-controllers: build
	$(CLI) find-controllers

get-controller: build
	$(CLI) get-controller --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

set-IPv4: build
	$(CLI) set-IPv4 --controller $(CONTROLLER) --address $(ADDRESS) --netmask $(NETMASK) --gateway $(GATEWAY)

get-time: build
	$(CLI) get-time --controller $(CONTROLLER)

set-time: build
	$(CLI) set-time --controller $(CONTROLLER)

get-listener: build
	$(CLI) get-listener --controller $(CONTROLLER)

set-listener: build
	$(CLI) set-listener --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --listener $(LISTENER) --interval $(INTERVAL)

get-status: build
	$(CLI) get-status --controller $(CONTROLLER)

get-door: build
	$(CLI) get-door --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --door $(DOOR)

set-door: build
	$(CLI) set-door --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --door $(DOOR) --mode $(MODE) --delay $(DELAY)

set-door-passcodes: build
	$(CLI) set-door-passcodes --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --door $(DOOR) --passcodes $(PASSCODES)

open-door: build
	$(CLI) open-door --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --door $(DOOR)

get-cards: build
	$(CLI) get-cards --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

get-card: build
	$(CLI) get-card --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --card $(CARD)

get-card-at-index: build
	$(CLI) get-card-at-index --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --index $(CARD_INDEX)

put-card: build
	$(CLI) put-card --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --card $(CARD) --start $(START_DATE) --end $(END_DATE) --doors $(DOORS) --PIN $(PIN)

delete-card: build
	$(CLI) delete-card --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --card $(CARD)

delete-all-cards: build
	$(CLI) delete-all-cards --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

get-event: build
	$(CLI) get-event --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --index $(EVENT_INDEX)

get-event-index: build
	$(CLI) get-event-index --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

set-event-index: build
	$(CLI) set-event-index --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --index $(EVENT_INDEX)

record-special-events: build
	$(CLI) record-special-events --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --enabled $(RECORD_SPECIAL_EVENTS)

get-time-profile: build
	$(CLI) get-time-profile --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --profile-id $(TIME_PROFILE_ID)

set-time-profile: build
	$(CLI) set-time-profile --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --profile-id $(TIME_PROFILE_ID) --profile "$(TIME_PROFILE)"

clear-time-profiles: build
	$(CLI) clear-time-profiles --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

add-task: build
	$(CLI) add-task --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --task "$(TASK)"

refresh-tasklist: build
	$(CLI) refresh-tasklist --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

clear-tasklist: build
	$(CLI) clear-tasklist --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

set-pc-control: build
	$(CLI) set-pc-control --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --enable $(PC_CONTROL)

set-interlock: build
	$(CLI) set-interlock --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --interlock "$(INTERLOCK)"

activate-keypads: build
	$(CLI) activate-keypads --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --keypads "$(KEYPADS)"

get-antipassback: build
	$(CLI) get-antipassback --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

set-antipassback: build
	$(CLI) set-antipassback --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --antipassback "$(ANTIPASSBACK)"

set-firstcard: build
	$(CLI) set-firstcard --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT) --door $(DOOR) --firstcard "$(FIRSTCARD)"

restore-default-parameters: build
	$(CLI) restore-default-parameters --controller $(CONTROLLER) --dest $(DEST) --transport $(TRANSPORT)

listen: build
	$(CLI) listen
