with Ada.Streams;
with System;

with Uhppoted.Lib.Types;

package Uhppoted.Lib.Responses is
   use Uhppoted.Lib.Types;

   type GetControllerResponse is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : MAC_Address;
      Version  : BCD (1 .. 2);
      Date     : BCD (1 .. 8);
      Reserved : Ada.Streams.Stream_Element_Array (1 .. 28);
   end record;

   for GetControllerResponse use record
      ID at 0 range  32 ..  63;
      Address at 0 range 64 .. 95;
      Netmask at 0 range 96 .. 127;
      Gateway at 0 range 128 .. 159;
      MAC at 0 range 160 .. 207;
      Version at 0 range 208 .. 223;
      Date at 0 range 224 .. 287;
   end record;

   for GetControllerResponse'Bit_Order use System.Low_Order_First;
   for GetControllerResponse'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Responses;
