with GNAT.Sockets;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Transport.TCP is
   use Uhppoted.Lib.Types;

   function Send (U        : UHPPOTE;
                  DestAddr : GNAT.Sockets.Sock_Addr_Type;
                  Request  : Packet;
                  Timeout  : Duration) return Packet;

end Uhppoted.Lib.Transport.TCP;
