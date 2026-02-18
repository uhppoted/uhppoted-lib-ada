with Uhppoted.Lib.Types;
with Uhppoted.Types;

package Uhppoted.Lib.Decode is
   use Uhppoted.Lib.Types;

   --  Decodes a 64 byte get-controller response as a Controller record.
   function Get_Controller (Reply : Packet) return Uhppoted.Types.Controller_Record;

private
   function BCD_To_String (Bytes : BCD) return String;

end Uhppoted.Lib.Decode;
