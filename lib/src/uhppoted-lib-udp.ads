with Uhppoted.Lib.Types;

package Uhppoted.Lib.UDP is
   use Uhppoted.Lib.Types;

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   function Broadcast (U : UHPPOTE; Request : Packet) return Packet_List;

   --  Sends a 64 byte request packet and returns the response (if any).
   function Send (U : UHPPOTE; Request : Packet) return Packet;

end Uhppoted.Lib.UDP;
