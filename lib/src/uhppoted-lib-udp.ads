with GNAT.Sockets;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.UDP is
   use Uhppoted.Lib.Types;

   function Broadcast (U : UHPPOTE; Request : Packet; Timeout : Duration) return Packet_List;
   function SendTo    (U        : UHPPOTE;
                       DestAddr : GNAT.Sockets.Sock_Addr_Type;
                       Request  : Packet;
                       Timeout  : Duration) return Packet;

end Uhppoted.Lib.UDP;
