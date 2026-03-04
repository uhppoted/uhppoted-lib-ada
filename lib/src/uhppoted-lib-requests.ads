with Ada.Streams;
with System;
with Uhppoted.Lib.Codec;

package Uhppoted.Lib.Requests is

   type BCD7 is array (1 .. 7) of Unsigned_8;

   type Get_Controller_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Controller;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Controller_Request use record
      SOM        at 0 range 0 .. 7;
      OpCode     at 1 range 0 .. 7;
      Reserved   at 2 range 0 .. 15;
      Controller at 4 range 0 .. 31;
      Padding    at 8 range 0 .. 447;
   end record;

   for Get_Controller_Request'Size use 64 * 8;
   for Get_Controller_Request'Bit_Order use System.Low_Order_First;
   for Get_Controller_Request'Scalar_Storage_Order use System.Low_Order_First;

   type Set_IPv4_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_IPv4;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Addr       : IPv4;
      Netmask    : IPv4;
      Gateway    : IPv4;
      MagicWord  : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 40) := [others => 0];
   end record;

   for Set_IPv4_Request use record
      SOM        at 0  range 0 .. 7;
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

   type Get_Time_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Time;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Time_Request use record
      SOM        at 0 range 0 .. 7;
      OpCode     at 1 range 0 .. 7;
      Reserved   at 2 range 0 .. 15;
      Controller at 4 range 0 .. 31;
      Padding    at 8 range 0 .. 447;
   end record;

   for Get_Time_Request'Size use 64 * 8;
   for Get_Time_Request'Bit_Order use System.Low_Order_First;
   for Get_Time_Request'Scalar_Storage_Order use System.Low_Order_First;

   type Set_Time_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Time;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Date_Time  : BCD7 := [others => 0];
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 49) := [others => 0];
   end record;

   for Set_Time_Request use record
      SOM        at 0  range 0 .. 7;
      OpCode     at 1  range 0 .. 7;
      Reserved   at 2  range 0 .. 15;
      Controller at 4  range 0 .. 31;
      Date_Time  at 8  range 0 .. 55;
      Padding    at 15 range 0 .. 391;
   end record;

   for Set_Time_Request'Size use 64 * 8;
   for Set_Time_Request'Bit_Order use System.Low_Order_First;
   for Set_Time_Request'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Requests;
