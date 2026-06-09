with Ada.Containers.Vectors;

--  Internal type definitions.
--

package Uhppoted.Lib.Types is
   --  64 byte message packet.
   type Packet is array (1 .. 64) of Unsigned_8;

   --  BCD byte array.
   type BCD is array (Positive range <>) of Unsigned_8;

   --  Internal implementation for Packet_List.
   package Packet_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Packet);

   --  Growable list of messages.
   subtype Packet_List is Packet_Vectors.Vector;

end Uhppoted.Lib.Types;
