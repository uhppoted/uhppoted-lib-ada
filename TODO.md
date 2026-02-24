# TODO

- [ ] format
- [ ] lint
- [x] find-controllers

- [ ] get-controller
    - [x] controller ID
    - [x] timeout parameter
    - [x] integration test
    - [x] controller struct arg
    - [x] CLI
    - [x] validate controller ID in response
    - [x] README
    - [x] API.md

- [ ] UDP
    - [x] broadcast timeout parameter
    - [x] send timeout parameter
    - [x] sendto
    - [ ] controlled types for sockets/selectors

- [ ] TCP

- [ ] CLI
    - [ ] command line args
    - [ ] handle exceptions
    - [ ] pretty print

- [ ] integration tests
    - [x] UDP
    - [x] search messages list
    - [ ] Figure out how to run both test suites
    - [ ] 'Expected' package
    - [ ] Invalid response test

- [ ] codegen
    - [x] encode unit tests
    - [x] decode unit tests
    - [ ] default integration tests

- [ ] fix _Library_Interface_ in .gpr file
```
   for Library_Interface use (
      "Uhppoted",
      "Uhppoted.Lib",
      "Uhppoted.Lib.Types",
      "Uhppoted.Lib.Decode"
   );
```

- [ ] use enums for OpCodes
```
type Hardware_Status is (Off, Standby, On, Error);

for Hardware_Status use (
   Off     => 16#00#,
   Standby => 16#01#,
   On      => 16#02#,
   Error   => 16#FF#
);
```

- [ ] Controller utility constructor
```
function To_Controller (
   ID       : Interfaces.Unsigned_32;
   Dest     : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
   Protocol : Protocol_Type               := Default
) return Controller is
begin
   return (
      Controller => ID,
      DestAddr   => Dest,
      Protocol   => Protocol
   );
end To_Controller;
```
