with GNAT.Sockets;
with Uhppoted.Lib.Types;

--  TCP transport implementation for the UHPPOTE request/response protocol.
--
--  Sends a UHPPOTE request packet over TCP socket and waits for the response.

package Uhppoted.Lib.Transport.TCP is
   use Uhppoted.Lib.Types;

   --  (weird gnatdoc bug: blank lines above and below required or param/field tags are rejected)

   --  Sends a 64 byte request packet to a specific IPv4 address:port and returns the response (if any).
   --
   --  @param  U         UHPPOTE configuration.
   --  @param  DestAddr  Controller IPv4 address:port.
   --  @param  Request   64 byte request packet.
   --  @param  Timeout   Operation timeout (defaults to 2.5s).
   --
   --  @return  Received packet.
   function Send
     (U : UHPPOTE; DestAddr : GNAT.Sockets.Sock_Addr_Type; Request : Packet; Timeout : Duration) return Packet;

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

end Uhppoted.Lib.Transport.TCP;
