with Ada.Streams;
with System;
with Uhppoted.Lib.Codec;

package Uhppoted.Lib.Requests is

   type BCD2 is array (1 .. 2) of Unsigned_8;
   type BCD4 is array (1 .. 4) of Unsigned_8;
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
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
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
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 40) := [others => 0];
   end record;

   for Set_IPv4_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Addr       at  8 range 0 ..  31;
      Netmask    at 12 range 0 ..  31;
      Gateway    at 16 range 0 ..  31;
      MagicWord  at 20 range 0 ..  31;
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
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
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
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Date_Time  at  8 range 0 ..  55;
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
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
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
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Addr       at  8 range 0 ..  31;
      Port       at 12 range 0 ..  15;
      Interval   at 14 range 0 ..   7;
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
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
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
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Door       at 8 range 0 ..   7;
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
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Door       at  8 range 0 ..   7;
      Mode       at  9 range 0 ..   7;
      OpenDelay  at 10 range 0 ..   7;
      Padding    at 11 range 0 .. 423;
   end record;

   for Set_Door_Request'Size use 64 * 8;
   for Set_Door_Request'Bit_Order use System.Low_Order_First;
   for Set_Door_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-door-passcodes request.
   type Set_Door_Passcodes_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Door_Passcodes;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Reserved2  : Ada.Streams.Stream_Element_Array (1 .. 3) := [others => 0];
      Passcode1  : Unsigned_32 := 0;
      Passcode2  : Unsigned_32 := 0;
      Passcode3  : Unsigned_32 := 0;
      Passcode4  : Unsigned_32 := 0;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 36) := [others => 0];
   end record;

   for Set_Door_Passcodes_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Door       at  8 range 0 ..   7;
      Reserved2  at  9 range 0 ..  23;
      Passcode1  at 12 range 0 ..  31;
      Passcode2  at 16 range 0 ..  31;
      Passcode3  at 20 range 0 ..  31;
      Passcode4  at 24 range 0 ..  31;
      Padding    at 28 range 0 .. 287;
   end record;

   for Set_Door_Passcodes_Request'Size use 64 * 8;
   for Set_Door_Passcodes_Request'Bit_Order use System.Low_Order_First;
   for Set_Door_Passcodes_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an open-door request.
   type Open_Door_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Open_Door;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Open_Door_Request use record
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Door       at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Open_Door_Request'Size use 64 * 8;
   for Open_Door_Request'Bit_Order use System.Low_Order_First;
   for Open_Door_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-cards request.
   type Get_Cards_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Cards;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Cards_Request use record
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Padding    at 8 range 0 .. 447;
   end record;

   for Get_Cards_Request'Size use 64 * 8;
   for Get_Cards_Request'Bit_Order use System.Low_Order_First;
   for Get_Cards_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-card request.
   type Get_Card_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Card;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Get_Card_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Card       at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Get_Card_Request'Size use 64 * 8;
   for Get_Card_Request'Bit_Order use System.Low_Order_First;
   for Get_Card_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-card-by-index request.
   type Get_Card_At_Index_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Card_At_Index;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Index      : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Get_Card_At_Index_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Index      at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Get_Card_At_Index_Request'Size use 64 * 8;
   for Get_Card_At_Index_Request'Bit_Order use System.Low_Order_First;
   for Get_Card_At_Index_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a put-card request.
   type Put_Card_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Put_Card;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      Start_Date : BCD4;
      End_Date   : BCD4;
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_24;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 37) := [others => 0];
   end record;

   for Put_Card_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Card       at  8 range 0 ..  31;
      Start_Date at 12 range 0 ..  31;
      End_Date   at 16 range 0 ..  31;
      Door_1     at 20 range 0 ..   7;
      Door_2     at 21 range 0 ..   7;
      Door_3     at 22 range 0 ..   7;
      Door_4     at 23 range 0 ..   7;
      PIN        at 24 range 0 ..  23;
      Padding    at 27 range 0 .. 295;
   end record;

   for Put_Card_Request'Size use 64 * 8;
   for Put_Card_Request'Bit_Order use System.Low_Order_First;
   for Put_Card_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a delete-card request.
   type Delete_Card_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Delete_Card;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Delete_Card_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Card       at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Delete_Card_Request'Size use 64 * 8;
   for Delete_Card_Request'Bit_Order use System.Low_Order_First;
   for Delete_Card_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a delete-cards request.
   type Delete_Cards_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Delete_All_Cards;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Delete_Cards_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      MagicWord  at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Delete_Cards_Request'Size use 64 * 8;
   for Delete_Cards_Request'Bit_Order use System.Low_Order_First;
   for Delete_Cards_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-event request.
   type Get_Event_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Event;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Index      : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Get_Event_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Index      at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Get_Event_Request'Size use 64 * 8;
   for Get_Event_Request'Bit_Order use System.Low_Order_First;
   for Get_Event_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-event-index request.
   type Get_Event_Index_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Event_Index;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 53) := [others => 0];
   end record;

   for Get_Event_Index_Request use record
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Padding    at 8 range 0 .. 423;
   end record;

   for Get_Event_Index_Request'Size use 64 * 8;
   for Get_Event_Index_Request'Bit_Order use System.Low_Order_First;
   for Get_Event_Index_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-event-index request.
   type Set_Event_Index_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Event_Index;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Index      : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48) := [others => 0];
   end record;

   for Set_Event_Index_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Index      at  8 range 0 ..  31;
      MagicWord  at 12 range 0 ..  31;
      Padding    at 16 range 0 .. 383;
   end record;

   for Set_Event_Index_Request'Size use 64 * 8;
   for Set_Event_Index_Request'Bit_Order use System.Low_Order_First;
   for Set_Event_Index_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a record-special-events request.
   type Record_Special_Events_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Record_Special_Events;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Enabled    : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Record_Special_Events_Request use record
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Enabled    at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Record_Special_Events_Request'Size use 64 * 8;
   for Record_Special_Events_Request'Bit_Order use System.Low_Order_First;
   for Record_Special_Events_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-time-profile request.
   type Get_Time_Profile_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Time_Profile;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Profile    : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Get_Time_Profile_Request use record
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Profile    at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Get_Time_Profile_Request'Size use 64 * 8;
   for Get_Time_Profile_Request'Bit_Order use System.Low_Order_First;
   for Get_Time_Profile_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-time-profile request.
   type Set_Time_Profile_Request is record
      SOM             : Unsigned_8    := Codec.SOM;
      OpCode          : Codec.Op_Code := Codec.Set_Time_Profile;
      Reserved        : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller      : Unsigned_32;
      Profile         : Unsigned_8;
      Start_Date      : BCD4;
      End_Date        : BCD4;
      Monday          : Unsigned_8;
      Tuesday         : Unsigned_8;
      Wednesday       : Unsigned_8;
      Thursday        : Unsigned_8;
      Friday          : Unsigned_8;
      Saturday        : Unsigned_8;
      Sunday          : Unsigned_8;
      Segment_1_Start : BCD2;
      Segment_1_End   : BCD2;
      Segment_2_Start : BCD2;
      Segment_2_End   : BCD2;
      Segment_3_Start : BCD2;
      Segment_3_End   : BCD2;
      Linked_Profile  : Unsigned_8;
      Padding         : Ada.Streams.Stream_Element_Array (1 .. 27) := [others => 0];
   end record;

   for Set_Time_Profile_Request use record
      SOM             at  0 range 0 ..   7;
      OpCode          at  1 range 0 ..   7;
      Reserved        at  2 range 0 ..  15;
      Controller      at  4 range 0 ..  31;
      Profile         at  8 range 0 ..   7;
      Start_Date      at  9 range 0 ..   31;
      End_Date        at 13 range 0 ..   31;
      Monday          at 17 range 0 ..   7;
      Tuesday         at 18 range 0 ..   7;
      Wednesday       at 19 range 0 ..   7;
      Thursday        at 20 range 0 ..   7;
      Friday          at 21 range 0 ..   7;
      Saturday        at 22 range 0 ..   7;
      Sunday          at 23 range 0 ..   7;
      Segment_1_Start at 24 range 0 ..  15;
      Segment_1_End   at 26 range 0 ..  15;
      Segment_2_Start at 28 range 0 ..  15;
      Segment_2_End   at 30 range 0 ..  15;
      Segment_3_Start at 32 range 0 ..  15;
      Segment_3_End   at 34 range 0 ..  15;
      Linked_Profile  at 36 range 0 ..   7;
      Padding         at 37 range 0 .. 215;
   end record;

   for Set_Time_Profile_Request'Size use 64 * 8;
   for Set_Time_Profile_Request'Bit_Order use System.Low_Order_First;
   for Set_Time_Profile_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a clear-time-profiles request.
   type Clear_Time_Profiles_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Clear_Time_Profiles;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Clear_Time_Profiles_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      MagicWord  at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Clear_Time_Profiles_Request'Size use 64 * 8;
   for Clear_Time_Profiles_Request'Bit_Order use System.Low_Order_First;
   for Clear_Time_Profiles_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an add-task request.
   type Add_Task_Request is record
      SOM             : Unsigned_8    := Codec.SOM;
      OpCode          : Codec.Op_Code := Codec.Add_Task;
      Reserved        : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller      : Unsigned_32;
      Start_Date      : BCD4;
      End_Date        : BCD4;
      Monday          : Unsigned_8;
      Tuesday         : Unsigned_8;
      Wednesday       : Unsigned_8;
      Thursday        : Unsigned_8;
      Friday          : Unsigned_8;
      Saturday        : Unsigned_8;
      Sunday          : Unsigned_8;
      Start_Time      : BCD2;
      Door            : Unsigned_8;
      Task_ID         : Task_Type;
      More_Cards      : Unsigned_8;
      Padding         : Ada.Streams.Stream_Element_Array (1 .. 35) := [others => 0];
   end record;

   for Add_Task_Request use record
      SOM         at  0 range 0 ..   7;
      OpCode      at  1 range 0 ..   7;
      Reserved    at  2 range 0 ..  15;
      Controller  at  4 range 0 ..  31;
      Start_Date  at  8 range 0 ..  31;
      End_Date    at 12 range 0 ..  31;
      Monday      at 16 range 0 ..   7;
      Tuesday     at 17 range 0 ..   7;
      Wednesday   at 18 range 0 ..   7;
      Thursday    at 19 range 0 ..   7;
      Friday      at 20 range 0 ..   7;
      Saturday    at 21 range 0 ..   7;
      Sunday      at 22 range 0 ..   7;
      Start_Time  at 23 range 0 ..  15;
      Door        at 25 range 0 ..   7;
      Task_ID     at 26 range 0 ..   7;
      More_Cards  at 27 range 0 ..   7;
      Padding     at 28 range 0 .. 279;
   end record;

   for Add_Task_Request'Size use 64 * 8;
   for Add_Task_Request'Bit_Order use System.Low_Order_First;
   for Add_Task_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a refresh-tasklist request.
   type Refresh_Task_List_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Refresh_Task_List;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Refresh_Task_List_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      MagicWord  at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Refresh_Task_List_Request'Size use 64 * 8;
   for Refresh_Task_List_Request'Bit_Order use System.Low_Order_First;
   for Refresh_Task_List_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a clear-tasklist request.
   type Clear_Task_List_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Clear_Task_List;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Clear_Task_List_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      MagicWord  at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Clear_Task_List_Request'Size use 64 * 8;
   for Clear_Task_List_Request'Bit_Order use System.Low_Order_First;
   for Clear_Task_List_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-pc-control request.
   type Set_PC_Control_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_PC_Control;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Enable     : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 51) := [others => 0];
   end record;

   for Set_PC_Control_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      MagicWord  at  8 range 0 ..  31;
      Enable     at 12 range 0 ..   7;
      Padding    at 13 range 0 .. 407;
   end record;

   for Set_PC_Control_Request'Size use 64 * 8;
   for Set_PC_Control_Request'Bit_Order use System.Low_Order_First;
   for Set_PC_Control_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-interlock request.
   type Set_Interlock_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Interlock;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Interlock  : Uhppoted.Lib.Interlock;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Set_Interlock_Request use record
      SOM        at 0 range 0 ..   7;
      OpCode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Interlock  at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Set_Interlock_Request'Size use 64 * 8;
   for Set_Interlock_Request'Bit_Order use System.Low_Order_First;
   for Set_Interlock_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an activate-keypads request.
   type Activate_Keypads_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Activate_Keypads;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Reader_1   : Unsigned_8;
      Reader_2   : Unsigned_8;
      Reader_3   : Unsigned_8;
      Reader_4   : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Activate_Keypads_Request use record
      SOM        at  0 range 0 ..  7;
      OpCode     at  1 range 0 ..  7;
      Reserved   at  2 range 0 .. 15;
      Controller at  4 range 0 .. 31;
      Reader_1   at  8 range 0 ..  7;
      Reader_2   at  9 range 0 ..  7;
      Reader_3   at 10 range 0 ..  7;
      Reader_4   at 11 range 0 ..  7;
      Padding    at 12 range 0 .. 415;
   end record;

   for Activate_Keypads_Request'Size use 64 * 8;
   for Activate_Keypads_Request'Bit_Order use System.Low_Order_First;
   for Activate_Keypads_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a restore-default-parameters request.
   type Restore_Default_Parameters_Request is record
      SOM        : Unsigned_8    := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Restore_Default_Parameters;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Restore_Default_Parameters_Request use record
      SOM        at  0 range 0 ..   7;
      OpCode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      MagicWord  at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Restore_Default_Parameters_Request'Size use 64 * 8;
   for Restore_Default_Parameters_Request'Bit_Order use System.Low_Order_First;
   for Restore_Default_Parameters_Request'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Requests;
