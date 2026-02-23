# TODO

- [ ] format
- [ ] lintx
- [x] find-controllers

- [ ] get-controller
    - [x] controller ID
    - [x] timeout parameter
    - [x] integration test
    - [x] controller struct arg
    - [ ] CLI
    - [ ] validate controller ID in response

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
    - [x] move listen to stub
    - [x] cleanup listen logic
    - [x] UDP
    - [ ] Figure out how to run both test suites
    - [ ] 'Expected' package
    - [ ] search messages list

- [ ] codegen
    - [x] encode unit tests
    - [ ] decode unit tests

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

```
type BCD_Version_Record is record
   Major_Tens  : Unsigned_4;
   Major_Units : Unsigned_4;
   Minor_Tens  : Unsigned_4;
   Minor_Units : Unsigned_4;
end record;

for BCD_Version_Record use record
   -- Adjust positions based on your hardware's endianness
   Major_Tens  at 0 range 12 .. 15;
   Major_Units at 0 range  8 .. 11;
   Minor_Tens  at 0 range  4 ..  7;
   Minor_Units at 0 range  0 ..  3;
end record;
for BCD_Version_Record'Size use 16;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

function Unpack_Version (V : Unsigned_16) return Unbounded_String is
   -- This function lets us treat the bits of V as our BCD record
   function To_BCD is new Ada.Unchecked_Conversion (Unsigned_16, BCD_Version_Record);
   
   Version : constant BCD_Version_Record := To_BCD (V);
   
   -- Simple mapping from 0..9 to '0'..'9'
   function Digit (Val : Unsigned_4) return Character is 
      (Character'Val (Character'Pos ('0') + Integer (Val)));
begin
   return To_Unbounded_String ("v" & 
                               Digit (Version.Major_Tens)  & 
                               Digit (Version.Major_Units) & "." & 
                               Digit (Version.Minor_Tens)  & 
                               Digit (Version.Minor_Units));
end Unpack_Version;
```


- https://stackoverflow.com/questions/67969309/ada-customise-image
- http://www.ada-auth.org/standards/22rm/html/rm-4-10.html
