with Uhppoted.Lib.Types;

package Uhppoted.Lib.Decode is
   use Uhppoted.Lib.Types;

   --  Decodes a 64 byte get-controller response as a Controller record.
   function Get_Controller (Reply : Packet)
      return Uhppoted.Lib.Controller;

private
   function BCD_To_String (Bytes : BCD) return String;

end Uhppoted.Lib.Decode;
