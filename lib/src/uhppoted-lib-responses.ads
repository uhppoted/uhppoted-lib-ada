with Interfaces;
with Ada.Streams;
with System;

with Uhppoted.Lib.Types;

package Uhppoted.Lib.Responses is
   use Interfaces;
   use Uhppoted.Lib.Types;

   type GetControllerResponse is record
      SOH        : Unsigned_8 := 16#17#;
      OpCode     : Unsigned_8 := 16#94#;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Address    : IPv4;
      Netmask    : IPv4;
      Gateway    : IPv4;
      MAC        : MAC_Address;
      Version    : BCD (1 .. 2);
      Date       : BCD (1 .. 4);
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 32);
   end record;

   for GetControllerResponse use record
      SOH        at 0  range 0 .. 7;
      OpCode     at 1  range 0 .. 7;
      Reserved   at 2  range 0 .. 15;
      Controller at 4  range 0 .. 31;
      Address    at 8  range 0 .. 31;
      Netmask    at 12 range 0 .. 31;
      Gateway    at 16 range 0 .. 31;
      MAC        at 20 range 0 .. 47;
      Version    at 26 range 0 .. 15;
      Date       at 28 range 0 .. 31;
      Padding    at 32 range 0 .. 255;
   end record;

   for GetControllerResponse'Size use 64 * 8;
   for GetControllerResponse'Bit_Order use System.Low_Order_First;
   for GetControllerResponse'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Responses;
