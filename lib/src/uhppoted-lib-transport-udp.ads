with GNAT.Sockets;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Transport.UDP is
   use Uhppoted.Lib.Types;

   function Broadcast   (U : UHPPOTE; Request : Packet; Timeout : Duration) return Packet_List;
   function BroadcastTo (U : UHPPOTE; Request : Packet; Timeout : Duration) return Packet;
   function SendTo      (U        : UHPPOTE;
                         DestAddr : GNAT.Sockets.Sock_Addr_Type;
                         Request  : Packet;
                         Timeout  : Duration) return Packet;

end Uhppoted.Lib.Transport.UDP;
