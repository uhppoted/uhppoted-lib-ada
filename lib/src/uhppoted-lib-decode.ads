with Uhppoted.Lib.Types;
with Uhppoted.Lib.Responses;

package Uhppoted.Lib.Decode is
   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   function Get_Controller (Reply : Packet) return Get_Controller_Response;

private
   function BCD_To_String (Bytes : BCD) return String;

end Uhppoted.Lib.Decode;
