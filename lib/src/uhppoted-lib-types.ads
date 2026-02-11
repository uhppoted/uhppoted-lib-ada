with Interfaces;
with Ada.Containers.Vectors;

package Uhppoted.Lib.Types is
   use Interfaces;

   type Packet is array (1 .. 64) of Unsigned_8;
   type BCD is array (Positive range <>) of Unsigned_8;

   package Packet_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Packet);

   subtype Packet_List is Packet_Vectors.Vector;

end Uhppoted.Lib.Types;
