# TODO

- [ ] format
- [ ] lint
- [ ] adadoc
- https://github.com/simonjwright/alire-index.mac
- https://alire.ada.dev/transition_from_gnat_community.html

- [x] use enums for OpCodes
- [x] find-controllers
    - [x] ada-doc

- [x] get-controller
    - [x] ada-doc

- [ ] set-IPv4
    - [x] API
    - [ ] integration test
    - [x] CLI
    - [x] README
    - [x] API.md
    - [x] ada-doc

- [ ] UDP
    - [x] broadcast timeout parameter
    - [x] send timeout parameter
    - [x] sendto
    - [ ] split send/sendto
    - [ ] listen
    - [ ] controlled types for sockets/selectors
    - [ ] ada-doc

- [x] TCP
    - [ ] controlled types for sockets/selectors
    - [ ] ada-doc

- [ ] CLI
    - [ ] command line args
    - [ ] handle exceptions
    - [ ] pretty print

- [ ] integration tests
    - [x] UDP
    - [x] TCP
    - [x] search messages list
    - [x] Figure out how to run both test suites
    - [ ] Fix selector logic for multiple tests
    - [ ] Fix listen timeout
    - [ ] 'Expected' package
    - [ ] Invalid response test
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

- Controlled type
```
with Ada.Finalization;
with GNAT.Sockets;

package body Uhppoted.Lib.Transport.TCP is
   use GNAT.Sockets;

   -- A helper record to manage the lifecycle of the network resources
   type Connection_Manager is new Ada.Finalization.Limited_Controlled with record
      Socket   : Socket_Type := No_Socket;
      Selector : Selector_Type;
      Created  : Boolean := False;
   end record;

   -- Automatically called when Connection_Manager goes out of scope
   overriding procedure Finalize (Self : in out Connection_Manager) is
   begin
      if Self.Created then
         Close_Selector (Self.Selector);
      end if;
      if Self.Socket /= No_Socket then
         Close_Socket (Self.Socket);
      end if;
   end Finalize;

   function Send (U        : UHPPOTE;
                  DestAddr : Sock_Addr_Type;
                  Request  : Packet;
                  Timeout  : Duration) return Packet is
      
      -- This object is initialized at the start and finalized at the end
      Manager : Connection_Manager;
      Offset  : Ada.Streams.Stream_Element_Offset;
      Status  : Selector_Status;
      
      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Buffer    : Ada.Streams.Stream_Element_Array (1 .. 64);
   begin
      -- Setup
      Create_Selector (Manager.Selector);
      Manager.Created := True;
      Create_Socket (Manager.Socket);

      -- Networking
      Bind_Socket (Manager.Socket, U.Bind_Addr);
      Connect_Socket (Manager.Socket, DestAddr);

      Send_Socket (Manager.Socket, To_Stream (Request), Offset);

      Empty (Read_Set);
      Empty (Write_Set);
      Set (Read_Set, Manager.Socket);

      Check_Selector (Manager.Selector, Read_Set, Write_Set, Status, Timeout);

      if Status /= Completed then
         raise Timeout_Error;
      end if;

      Receive_Socket (Manager.Socket, Buffer, Offset);

      return To_Packet (Buffer);
      
      -- No 'exception' block or manual 'Close' calls needed.
      -- Ada handles the cleanup of 'Manager' automatically here.
   end Send;

end Uhppoted.Lib.Transport.TCP;
```
