with GNAT.Sockets;
with Uhppoted.Lib.Types;

--  @summary
--  TCP transport implementation for the UHPPOTE request/response protocol.
--
--  @description
--  Implements the functionality required to send a UHPPOTE request packet over TCP/IP
--  and wait for the response.
package Uhppoted.Lib.Transport.TCP is
   use Uhppoted.Lib.Types;

   function Send (U        : UHPPOTE;
                  DestAddr : GNAT.Sockets.Sock_Addr_Type;
                  Request  : Packet;
                  Timeout  : Duration) return Packet;
   --  Sends a 64 byte request packet to a specific IPv4 address:port and returns the response (if any).
   --
   --  @param  DestAddr  Controller IPv4 address:port.
   --  @param  Request   64 byte request packet.
   --  @param  Timeout   Operation timeout (defaults to 2.5s).

   type S is tagged limited private;
   --  Controlled_Type wrapper for GNAT.Socket.

private
   type S is new Ada.Finalization.Limited_Controlled with record
      Client : Socket_Type := GNAT.Sockets.No_Socket;
   end record;
   --  Controlled_Type wrapper for GNAT.Socket.

   overriding procedure Initialize (E : in out S);
   --  Creates the wrapped socket handle.

   overriding procedure Finalize (E : in out S);
   --  Closes the wrapped socket handle.

end Uhppoted.Lib.Transport.TCP;
