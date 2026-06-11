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

end Uhppoted.Lib.Transport.TCP;
