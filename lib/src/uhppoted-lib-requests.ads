with Ada.Streams;
with System;
with Uhppoted.Types;

package Uhppoted.Lib.Requests is

   type Get_Controller_Request is record
      SOH        : Unsigned_8 := 16#17#;
      OpCode     : Unsigned_8 := 16#94#;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Controller_Request use record
      SOH        at 0 range 0 .. 7;
      OpCode     at 1 range 0 .. 7;
      Reserved   at 2 range 0 .. 15;
      Controller at 4 range 0 .. 31;
      Padding    at 8 range 0 .. 447;
   end record;

   for Get_Controller_Request'Size use 64 * 8;
   for Get_Controller_Request'Bit_Order use System.Low_Order_First;
   for Get_Controller_Request'Scalar_Storage_Order use System.Low_Order_First;

   type Set_IPv4_Request is record
      SOH        : Unsigned_8 := 16#17#;
      OpCode     : Unsigned_8 := 16#96#;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Addr       : IPv4;
      Netmask    : IPv4;
      Gateway    : IPv4;
      MagicWord  : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 40) := [others => 0];
   end record;

   for Set_IPv4_Request use record
      SOH        at 0  range 0 .. 7;
      OpCode     at 1  range 0 .. 7;
      Reserved   at 2  range 0 .. 15;
      Controller at 4  range 0 .. 31;
      Addr       at 8  range 0 .. 31;
      Netmask    at 12 range 0 .. 31;
      Gateway    at 16 range 0 .. 31;
      MagicWord  at 20 range 0 .. 31;
      Padding    at 24 range 0 .. 319;
   end record;

   for Set_IPv4_Request'Size use 64 * 8;
   for Set_IPv4_Request'Bit_Order use System.Low_Order_First;
   for Set_IPv4_Request'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Requests;
