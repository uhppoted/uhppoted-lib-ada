with Ada.Streams;
with System;
with Uhppoted.Lib.Codec;

package Uhppoted.Lib.Requests is

   type BCD7 is array (1 .. 7) of Unsigned_8;

   --  Message definition for a get-controller request.
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

   --  Message definition for a set-IPv4 request.
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

   --  Message definition for a get-time request.
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

   --  Message definition for a set-time request.
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

   --  Message definition for a get-listener request.
   type Get_Listener_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Listener;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Listener_Request use record
      SOM        at 0 range 0 .. 7;
      OpCode     at 1 range 0 .. 7;
      Reserved   at 2 range 0 .. 15;
      Controller at 4 range 0 .. 31;
      Padding    at 8 range 0 .. 447;
   end record;

   for Get_Listener_Request'Size use 64 * 8;
   for Get_Listener_Request'Bit_Order use System.Low_Order_First;
   for Get_Listener_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-listener request.
   type Set_Listener_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Listener;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Addr       : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 49) := [others => 0];
   end record;

   for Set_Listener_Request use record
      SOM        at 0  range 0 .. 7;
      OpCode     at 1  range 0 .. 7;
      Reserved   at 2  range 0 .. 15;
      Controller at 4  range 0 .. 31;
      Addr       at 8  range 0 .. 31;
      Port       at 12 range 0 .. 15;
      Interval   at 14 range 0 .. 7;
      Padding    at 15 range 0 .. 391;
   end record;

   for Set_Listener_Request'Size use 64 * 8;
   for Set_Listener_Request'Bit_Order use System.Low_Order_First;
   for Set_Listener_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-status request.
   type Get_Status_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Status;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Status_Request use record
      SOM        at 0 range 0 .. 7;
      OpCode     at 1 range 0 .. 7;
      Reserved   at 2 range 0 .. 15;
      Controller at 4 range 0 .. 31;
      Padding    at 8 range 0 .. 447;
   end record;

   for Get_Status_Request'Size use 64 * 8;
   for Get_Status_Request'Bit_Order use System.Low_Order_First;
   for Get_Status_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-door request.
   type Get_Door_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Door;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Get_Door_Request use record
      SOM        at 0 range 0 .. 7;
      OpCode     at 1 range 0 .. 7;
      Reserved   at 2 range 0 .. 15;
      Controller at 4 range 0 .. 31;
      Door       at 8 range 0 .. 7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Get_Door_Request'Size use 64 * 8;
   for Get_Door_Request'Bit_Order use System.Low_Order_First;
   for Get_Door_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-door request.
   type Set_Door_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Door;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Control_Mode;
      OpenDelay  : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 53) := [others => 0];
   end record;

   for Set_Door_Request use record
      SOM        at 0  range 0 .. 7;
      OpCode     at 1  range 0 .. 7;
      Reserved   at 2  range 0 .. 15;
      Controller at 4  range 0 .. 31;
      Door       at 8  range 0 .. 7;
      Mode       at 9  range 0 .. 7;
      OpenDelay  at 10 range 0 .. 7;
      Padding    at 11 range 0 .. 423;
   end record;

   for Set_Door_Request'Size use 64 * 8;
   for Set_Door_Request'Bit_Order use System.Low_Order_First;
   for Set_Door_Request'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Requests;
