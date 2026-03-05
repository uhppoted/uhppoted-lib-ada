# TODO

- [ ] format
- [ ] lint
- [ ] adadoc
- https://github.com/ohenley/awesome-ada
- https://github.com/simonjwright/alire-index.mac
- https://alire.ada.dev/transition_from_gnat_community.html

- [x] use enums for OpCodes
- [ ] check opcode in decode

- [x] find-controllers
- [x] get-controller
- [x] set-IPv4
- [x] get-time
- [ ] set-time
    - [x] integration test
    - [ ] CLI: set time for _now_

- [ ] UDP
    - [x] broadcast timeout parameter
    - [x] send timeout parameter
    - [x] sendto
    - [x] split broadcast-to and send-to
    - [x] controlled types for sockets
    - [ ] controlled types for selectors
    - [ ] ada-doc
    - [ ] listen
    - [ ] fix selector logic
    - [ ] dump

- [x] TCP
    - [ ] controlled types for sockets/selectors
    - [ ] ada-doc
    - [ ] fix selector logic
    - [ ] dump

- [ ] CLI
    - [ ] command line args
    - [ ] handle exceptions
    - [ ] pretty print

- [ ] integration tests
    - [x] UDP
    - [x] TCP
    - [x] search messages list
    - [x] Figure out how to run both test suites
    - [x] Fix selector logic for multiple tests
    - [x] Fix listen timeout
    - [x] 'Expected' package
    - [ ] Invalid response test
    - [ ] Timeout test
    - [ ] `None` response timeout closes stub socket
    - [ ] v6.62

- [ ] codegen
    - [x] encode unit tests
    - [x] decode unit tests
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
