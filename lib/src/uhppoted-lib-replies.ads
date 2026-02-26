with Ada.Streams;
with System;

with Uhppoted.Lib.Types;
with Uhppoted.Lib.Codec;

package Uhppoted.Lib.Replies is
   use Interfaces;
   use Uhppoted.Lib.Types;

   type Version_Field is record
      Major : Unsigned_8;
      Minor : Unsigned_8;
   end record;

   for Version_Field use record
      Major at 0 range 0 .. 7;
      Minor at 0 range 8 .. 15;
   end record;

   for Version_Field'Size use 16;

   type Get_Controller_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Controller;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Address    : IPv4;
      Netmask    : IPv4;
      Gateway    : IPv4;
      MAC        : Hardware_Addr;
      Version    : Version_Field;
      Date       : BCD (1 .. 4);
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 32);
   end record;

   for Get_Controller_Response use record
      SOM        at 0  range 0 .. 7;
      Opcode     at 1  range 0 .. 7;
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

   for Get_Controller_Response'Size use 64 * 8;
   for Get_Controller_Response'Bit_Order use System.Low_Order_First;
   for Get_Controller_Response'Scalar_Storage_Order use System.Low_Order_First;

   type Set_IPv4_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Set_IPv4;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_IPv4_Response use record
      SOM        at 0 range 0 .. 7;
      Opcode     at 1 range 0 .. 7;
      Reserved   at 2 range 0 .. 15;
      Controller at 4 range 0 .. 31;
      Ok         at 8 range 0 .. 7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Set_IPv4_Response'Size use 64 * 8;
   for Set_IPv4_Response'Bit_Order use System.Low_Order_First;
   for Set_IPv4_Response'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Replies;
