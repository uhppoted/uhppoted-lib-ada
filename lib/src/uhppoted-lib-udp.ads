with Uhppoted.Lib.Types;

package Uhppoted.Lib.UDP is
   use Uhppoted.Lib.Types;

   function Broadcast (U : UHPPOTE; Request : Packet; Timeout : Duration) return Packet_List;
   function Send      (U : UHPPOTE; Request : Packet; Timeout : Duration) return Packet;

end Uhppoted.Lib.UDP;
