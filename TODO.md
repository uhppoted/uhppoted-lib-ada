# TODO

- https://github.com/ohenley/awesome-ada
- https://github.com/simonjwright/alire-index.mac
- https://alire.ada.dev/transition_from_gnat_community.html

- [ ] format
- [ ] lint
     - https://www.adalog.fr/en/adacontrol.html#download
- [ ] adadoc
    - [ ] exceptions
    - [ ] types
    - [ ] codec

- [x] TCP

- [x] find-controllers
- [x] get-controller
- [x] set-IPv4
- [x] get-time
- [x] set-time
- [x] get-status
- [x] get-listener
- [x] set-listener
- [x] get-door
- [x] set-door
- [x] set-door-passcodes
- [x] open-door
- [x] get-cards
- [x] get-card
- [x] get-card-by-index
- [ ] put-card
    - [x] API
    - [ ] CLI
       - [ ] --doors
    - [x] README
    - [ ] integration tests

- [x] check SOM in decode
- [x] check opcode in decode
- [ ] use Calendar.Date and Calendar.Time instead of Date/DateTime
- [ ] enums for event-type, event-reason, event-direction
- [ ] rename
     - (?) Controller_Record => Controller_Type
     - (?) Controller_Status => Status_Type
     - (?) Listener_Record => Listener_Type
     - (?) Door_Record => Door_Type
     - (?) Card_Record => Card_Type

- [ ] UDP
    - [x] dump
    - [x] connect
    - [x] send loop
    - [x] ada-doc
    - [x] check dest-addr is not bind addr
    - [ ] listen


- [ ] CLI
    - [x] command line args
    - [x] handle exceptions
    - [ ] pretty print

- [ ] integration tests
    - [x] `None` response timeout closes stub socket
    - [ ] Invalid response test
        - [ ] invalid SOM
        - [ ] invalid op-code
    - [ ] Timeout test
    - [ ] Connection refused test
    - [ ] v6.62

- [ ] fix _Library_Interface_ in .gpr file
```
   for Library_Interface use (
      "Uhppoted",
      "Uhppoted.Lib",
      "Uhppoted.Lib.Types",
      "Uhppoted.Lib.Decode"
   );
```
