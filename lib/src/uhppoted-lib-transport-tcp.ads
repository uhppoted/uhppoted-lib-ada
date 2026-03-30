with GNAT.Sockets;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Transport.TCP is
   use Uhppoted.Lib.Types;

   --  Sends a 64 byte request packet to a specific IPv4 address:port and returns the response (if any).
   --
   --  @param  DestAddr  Controller IPv4 address:port.
   --  @param  Request   64 byte request packet.
   --  @param  Timeout   Operation timeout (defaults to 2.5s).
   function Send (U        : UHPPOTE;
                  DestAddr : GNAT.Sockets.Sock_Addr_Type;
                  Request  : Packet;
                  Timeout  : Duration) return Packet;

   --  Controlled_Type wrapper for GNAT.Socket.
   type S is tagged limited private;

private
   --  Controlled_Type wrapper for GNAT.Socket.
   type S is new Ada.Finalization.Limited_Controlled with record
      Client : Socket_Type := GNAT.Sockets.No_Socket;
   end record;

   --  Creates the wrapped socket handle.
   overriding procedure Initialize (E : in out S);

   --  Closes the wrapped socket handle.
   overriding procedure Finalize (E : in out S);

end Uhppoted.Lib.Transport.TCP;
