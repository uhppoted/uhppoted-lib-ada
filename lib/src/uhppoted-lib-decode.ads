with Uhppoted.Lib.Types;
with Uhppoted.Lib.Responses;

package Uhppoted.Lib.Decode is
   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   function Get_Controller (Reply : Packet) return Get_Controller_Response;
   function Set_IPv4       (Reply : Packet) return Set_IPv4_Response;
   function Get_Time       (Reply : Packet) return Get_Time_Response;

end Uhppoted.Lib.Decode;
