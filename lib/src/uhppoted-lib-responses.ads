with Interfaces;
with Ada.Strings.Unbounded;

package Uhppoted.Lib.Responses is
   use Interfaces;
   use Ada.Strings.Unbounded;

   type Get_Controller_Response is record
      Controller  : Unsigned_32;
      IP_Address  : IPv4;
      Subnet_Mask : IPv4;
      Gateway     : IPv4;
      MAC_Address : Hardware_Addr;
      Version     : Unbounded_String;
      Date        : DateOnly;
   end record;

end Uhppoted.Lib.Responses;
