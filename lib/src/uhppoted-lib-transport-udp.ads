with GNAT.Sockets;
with Uhppoted.Lib.Types;

--  UDP transport implementation.
--
--  Send a UHPPOTE request packet over either UDP broadcast or a connected UDP socket
--  and waits for the response.

package Uhppoted.Lib.Transport.UDP is

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  Request  64 byte request packet.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          List of packets received within the timeout.
   function Broadcast
     (U : UHPPOTE; Request : Uhppoted.Lib.Types.Packet; Timeout : Duration) return Uhppoted.Lib.Types.Packet_List;

   --  Broadcasts a 64 byte request packet to a specific controller and returns the response (if any).
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  Request  64 byte request packet.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return  Received packet.
   function BroadcastTo
     (U : UHPPOTE; Request : Uhppoted.Lib.Types.Packet; Timeout : Duration) return Uhppoted.Lib.Types.Packet;

   --  Sends a 64 byte request packet to a specific controller over 'connectedf UDP' and returns the response (if any).
   --
   --  @param  U         UHPPOTE configuration.
   --  @param  DestAddr  Controller IPv4 address:port.
   --  @param  Request   64 byte request packet.
   --  @param  Timeout   Operation timeout (defaults to 2.5s).
   --
   --  @return          Received packet.
   function SendTo
     (U : UHPPOTE; DestAddr : GNAT.Sockets.Sock_Addr_Type; Request : Uhppoted.Lib.Types.Packet; Timeout : Duration)
      return Uhppoted.Lib.Types.Packet;

   --  Establishes a listening socket to receive controller events.
   --
   --  @param  U  UHPPOTE configuration.
   --  @param  X  Event handler for received events.
   --  @param  H  Selector wrapped in a Controlled type.
   procedure Listen (U : UHPPOTE; X : Event_Dispatcher'Class; H : GNAT.Sockets.Selector_Type);

   --  Controlled_Type wrapper for GNAT.Socket.
   type S is tagged limited private;

private
   --  Controlled_Type wrapper for GNAT.Socket.
   --
   --  @field  Client  Wrapped socket.
   type S is new Ada.Finalization.Limited_Controlled with record
      Client : Socket_Type := GNAT.Sockets.No_Socket;
   end record;

   --  Initialises the wrapped socket handle.
   --
   --  @param E  Controlled.Initialize implementation for socket.
   overriding
   procedure Initialize (E : in out S);

   --  Closes the wrapped socket handle.
   --
   --  @param E  Controlled.Finalize implementation for socket.
   overriding
   procedure Finalize (E : in out S);

   type Socket_Array is array (1 .. 5) of Socket_Type;

   --  Controlled_Type wrapper for list of GNAT.Socket.
   --
   --  @field  Client  Wrapped socket.
   type Socket_List is new Ada.Finalization.Limited_Controlled with record
      Clients : Socket_Array := [others => GNAT.Sockets.No_Socket];
      IX      : Positive := 1;
   end record;

   --  Initialises the wrapped list of sockets.
   --
   --  @param E  Controlled.Initialize implementation for socket list.
   overriding
   procedure Initialize (E : in out Socket_List);

   --  Closes the sockets in the wrapped list of sockets.
   --
   --  @param E  Controlled.Finalize implementation for socket list.
   overriding
   procedure Finalize (E : in out Socket_List);

   --  Returns either the active socket (if any) or No_Socket.
   function Client (E : Socket_List) return Socket_Type;

   --  Creates a new socket, adds it to the list of managed sockets and makes it the active socket.
   procedure Next (E : in out Socket_List);

end Uhppoted.Lib.Transport.UDP;
