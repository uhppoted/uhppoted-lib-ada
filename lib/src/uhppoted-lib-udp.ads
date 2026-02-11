with Uhppoted.Lib.Types;

package Uhppoted.Lib.UDP is
   use Uhppoted.Lib.Types;

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   function Broadcast (U : UHPPOTE; Request : Packet) return Packet_List;

end Uhppoted.Lib.UDP;
