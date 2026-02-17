with Ada.Streams;
with System;

package Uhppoted.Lib.Requests is

   type GetControllerRequest is record
      SOH        : Unsigned_8 := 16#17#;
      OpCode     : Unsigned_8 := 16#94#;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for GetControllerRequest use record
      SOH        at 0 range 0 .. 7;
      OpCode     at 1 range 0 .. 7;
      Reserved   at 2 range 0 .. 15;
      Controller at 4 range 0 .. 31;
      Padding    at 8 range 0 .. 447;
   end record;

   for GetControllerRequest'Size use 64 * 8;
   for GetControllerRequest'Bit_Order use System.Low_Order_First;
   for GetControllerRequest'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Requests;
