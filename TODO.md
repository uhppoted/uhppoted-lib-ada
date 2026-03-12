# TODO

- [ ] format
- [ ] lint
- [ ] adadoc
- https://github.com/ohenley/awesome-ada
- https://github.com/simonjwright/alire-index.mac
- https://alire.ada.dev/transition_from_gnat_community.html

- [x] find-controllers
- [x] get-controller
- [x] set-IPv4
- [x] get-time
- [x] set-time
- [x] get-status
- [ ] get-listener
    - [ ] integration tests

- [ ] check SOM in decode
- [ ] check opcode in decode
- [ ] use Calendar.Date and Calendar.Time instead of Date/DateTime
- [ ] enums for event-type, event-reason, event-direction
- [ ] rename
     - [ ] Controller_Record => Controller_Type
     - [ ] Controller_Status => Status_Type
     - [ ] Listener_Record => Listener_Type

- [ ] UDP
    - [x] controlled types for sockets
    - [x] controlled types for selectors
    - [x] fix selector logic
    - [x] dump
       - [x] received
       - [x] send
    - [ ] Connect
    - [ ] send loop
    - [ ] listen
    - [ ] ada-doc

- [x] TCP
    - [x] controlled types for sockets
    - [x] controlled types for selectors
    - [x] ~~fix selector logic~~
    - [x] dump
       - [x] received
       - [x] send
    - [ ] send loop
    - [ ] ada-doc

- [ ] CLI
    - [ ] command line args
    - [ ] handle exceptions
    - [ ] pretty print

- [ ] integration tests
    - [ ] Invalid response test
        - [ ] invalid SOM
        - [ ] invalid op-code
    - [ ] Timeout test
    - [ ] `None` response timeout closes stub socket
    - [ ] v6.62

- [ ] codegen
    - [ ] default integration tests
    - [ ] UDP integration tests
    - [ ] TCP integration tests

- [ ] fix _Library_Interface_ in .gpr file
```
   for Library_Interface use (
      "Uhppoted",
      "Uhppoted.Lib",
      "Uhppoted.Lib.Types",
      "Uhppoted.Lib.Decode"
   );
```

```
package ArgParse is
   -- Use a generic name here because the context is provided by the package
   type Command is (Find_Controllers, Set_IPv4, Get_Time);

   -- Every command has its own "Context" or "Params"
   package Params is
      type IPv4_Data is record
         Addr : String (1 .. 15);
      end record;

      type Time_Data is record
         Seconds : Integer;
      end record;
   end Params;

   -- Now your variant record is clean
   type Entry_Record (Kind : Command) is record
      case Kind is
         when Set_IPv4 => IP   : Params.IPv4_Data;
         when Set_Time => Time : Params.Time_Data;
         when others   => null;
      end case;
   end record;
end ArgParse;
```
