# TODO

- https://github.com/ohenley/awesome-ada
- https://github.com/simonjwright/alire-index.mac

- [ ] format
- [ ] lint
     - https://www.adalog.fr/en/adacontrol.html#download
- [ ] adadoc
    - [ ] exceptions
    - [ ] types
    - [ ] codec

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
- [x] put-card
- [x] delete-card
- [x] delete-cards
- [x] get-event-index
    - [x] API
    - [x] CLI
    - [x] README
    - [x] integration tests

- [ ] use Calendar.Date and Calendar.Time instead of Date/DateTime
- [ ] enums for event-type, event-reason, event-direction
- [ ] rename
     - (?) Controller_Record => Controller_Type
     - (?) Controller_Status => Status_Type
     - (?) Listener_Record => Listener_Type
     - (?) Door_Record => Door_Type
     - (?) Card_Record => Card_Type

- [ ] UDP
    - [ ] listen

- [ ] integration tests
    - [x] `None` response timeout closes stub socket
    - [x] Invalid response test
    - [x] Timeout test
    - [x] Connection refused test
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
