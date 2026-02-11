with Interfaces;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Encode is
   use Interfaces;
   use Uhppoted.Lib.Types;

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (ID : Unsigned_32) return Packet;

end Uhppoted.Lib.Encode;
