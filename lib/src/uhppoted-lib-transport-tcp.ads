with GNAT.Sockets;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Transport.TCP is
   use Uhppoted.Lib.Types;

   function Send (U        : UHPPOTE;
                  DestAddr : GNAT.Sockets.Sock_Addr_Type;
                  Request  : Packet;
                  Timeout  : Duration) return Packet;

   type S is tagged limited private;

private
   type S is new Ada.Finalization.Limited_Controlled with
   record
      Client : Socket_Type := GNAT.Sockets.No_Socket;
   end record;

   overriding
   procedure Initialize (E : in out S);

   overriding
   procedure Finalize (E : in out S);

end Uhppoted.Lib.Transport.TCP;
