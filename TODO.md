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
- [x] set-event-index
- [x] record-special-events
- [x] get-event
    - [x] API
    - [x] CLI
    - [x] README
    - [x] integration tests

- [ ] use Calendar.Date and Calendar.Time instead of Date/DateTime
- [ ] enums for event-type, event-reason, event-direction
- [ ] rename
     - [x] Card_Record => Card_Type
     - [x] Door_Record => Door_Type
     - (?) Controller_Record => Controller_Type
     - (?) Controller_Status => Controller_Status_Type
     - (?) Listener_Record => Listener_Type

- [ ] UDP
    - [ ] listen

- [ ] integration tests
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

- [ ] `clang: warning: overriding deployment version from '16.0' to '26.0' [-Woverriding-deployment-version]`
   - https://forum.ada-lang.io/t/gnat-toolchain-now-broken-after-os-upgrade-to-macos-26-tahoe-arm64/3843/8
   - https://gcc.gnu.org/bugzilla/show_bug.cgi?id=120645
   - https://github.com/alire-project/GNAT-FSF-builds/issues/94