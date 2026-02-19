with Interfaces;
with System;
with Ada.Streams;

with Uhppoted.Lib.Types;

package Uhppoted.Lib.Responses is
   use Interfaces;
   use Uhppoted.Lib.Types;

   type Get_Controller_Response is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : MAC_Address;
      Firmware : Version;
      Date     : DateOnly;
   end record;

end Uhppoted.Lib.Responses;
