with Ada.Streams;
with System;

with Uhppoted.Lib.Types;
with Uhppoted.Lib.Codec;

package Uhppoted.Lib.Replies is
   type Version_Field is record
      Major : Unsigned_8;
      Minor : Unsigned_8;
   end record;

   for Version_Field use record
      Major at 0 range 0 ..  7;
      Minor at 0 range 8 .. 15;
   end record;

   for Version_Field'Size use 16;

   --  Message definition for a get-controller reply.
   type Get_Controller_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Controller;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Address    : IPv4;
      Netmask    : IPv4;
      Gateway    : IPv4;
      MAC        : Hardware_Addr;
      Version    : Version_Field;
      Date       : Uhppoted.Lib.Types.BCD (1 .. 4);
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 32);
   end record;

   for Get_Controller_Reply use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Address    at  8 range 0 ..  31;
      Netmask    at 12 range 0 ..  31;
      Gateway    at 16 range 0 ..  31;
      MAC        at 20 range 0 ..  47;
      Version    at 26 range 0 ..  15;
      Date       at 28 range 0 ..  31;
      Padding    at 32 range 0 .. 255;
   end record;

   for Get_Controller_Reply'Size use 64 * 8;
   for Get_Controller_Reply'Bit_Order use System.Low_Order_First;
   for Get_Controller_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-IPv4 reply.
   type Set_IPv4_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Set_IPv4;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_IPv4_Reply use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Set_IPv4_Reply'Size use 64 * 8;
   for Set_IPv4_Reply'Bit_Order use System.Low_Order_First;
   for Set_IPv4_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-time reply.
   type Get_Time_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Time;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Date_Time  : Uhppoted.Lib.Types.BCD (1 .. 7);
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48);
   end record;

   for Get_Time_Response use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Date_Time  at  8 range 0 ..  55;
      Padding    at 15 range 0 .. 383;
   end record;

   for Get_Time_Response'Size use 64 * 8;
   for Get_Time_Response'Bit_Order use System.Low_Order_First;
   for Get_Time_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-time reply.
   type Set_Time_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Set_Time;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Date_Time  : Uhppoted.Lib.Types.BCD (1 .. 7);
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48);
   end record;

   for Set_Time_Response use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Date_Time  at  8 range 0 ..  55;
      Padding    at 15 range 0 .. 383;
   end record;

   for Set_Time_Response'Size use 64 * 8;
   for Set_Time_Response'Bit_Order use System.Low_Order_First;
   for Set_Time_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-listener reply.
   type Get_Listener_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Listener;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Address    : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48);
   end record;

   for Get_Listener_Response use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Address    at  8 range 0 ..  31;
      Port       at 12 range 0 ..  15;
      Interval   at 14 range 0 ..   7;
      Padding    at 15 range 0 .. 383;
   end record;

   for Get_Listener_Response'Size use 64 * 8;
   for Get_Listener_Response'Bit_Order use System.Low_Order_First;
   for Get_Listener_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-listener-addrport reply.
   type Get_Listener_Addr_Port_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Listener;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Address    : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48);
   end record;

   for Get_Listener_Addr_Port_Response use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Address    at  8 range 0 ..  31;
      Port       at 12 range 0 ..  15;
      Interval   at 14 range 0 ..   7;
      Padding    at 15 range 0 .. 383;
   end record;

   for Get_Listener_Addr_Port_Response'Size use 64 * 8;
   for Get_Listener_Addr_Port_Response'Bit_Order use System.Low_Order_First;
   for Get_Listener_Addr_Port_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-listener reply.
   type Set_Listener_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Set_Listener;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Boolean;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Listener_Response use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Set_Listener_Response'Size use 64 * 8;
   for Set_Listener_Response'Bit_Order use System.Low_Order_First;
   for Set_Listener_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-listener reply.
   type Set_Listener_Addr_Port_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Set_Listener;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Boolean;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Listener_Addr_Port_Response use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Set_Listener_Addr_Port_Response'Size use 64 * 8;
   for Set_Listener_Addr_Port_Response'Bit_Order use System.Low_Order_First;
   for Set_Listener_Addr_Port_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-status reply.
   type Get_Status_Response is record
      SOM                  : Unsigned_8    := Codec.SOM;
      Opcode               : Codec.Op_Code := Codec.Get_Status;
      Reserved             : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller           : Unsigned_32;
      Event_Index          : Unsigned_32;
      Event_Type           : Unsigned_8;
      Event_Access_Granted : Unsigned_8;
      Event_Door           : Unsigned_8;
      Event_Direction      : Unsigned_8;
      Event_Card           : Unsigned_32;
      Event_Timestamp      : Uhppoted.Lib.Types.BCD (1 .. 7);
      Event_Reason         : Unsigned_8;
      Door_1_Open          : Unsigned_8;
      Door_2_Open          : Unsigned_8;
      Door_3_Open          : Unsigned_8;
      Door_4_Open          : Unsigned_8;
      Door_1_Button        : Unsigned_8;
      Door_2_Button        : Unsigned_8;
      Door_3_Button        : Unsigned_8;
      Door_4_Button        : Unsigned_8;
      System_Error         : Unsigned_8;
      System_Date          : Uhppoted.Lib.Types.BCD (1 .. 3);
      System_Time          : Uhppoted.Lib.Types.BCD (1 .. 3);
      Sequence_No          : Unsigned_32;
      Special_Info         : Unsigned_8;
      Relays               : Unsigned_8;
      Inputs               : Unsigned_8;
      Padding              : Ada.Streams.Stream_Element_Array (1 .. 10);
   end record;

   for Get_Status_Response use record
      SOM                  at  0 range 0 ..  7;
      Opcode               at  1 range 0 ..  7;
      Reserved             at  2 range 0 .. 15;
      Controller           at  4 range 0 .. 31;
      Event_Index          at  8 range 0 .. 31;
      Event_Type           at 12 range 0 ..  7;
      Event_Access_Granted at 13 range 0 ..  7;
      Event_Door           at 14 range 0 ..  7;
      Event_Direction      at 15 range 0 ..  7;
      Event_Card           at 16 range 0 .. 31;
      Event_Timestamp      at 20 range 0 .. 55;
      Event_Reason         at 27 range 0 ..  7;
      Door_1_Open          at 28 range 0 ..  7;
      Door_2_Open          at 29 range 0 ..  7;
      Door_3_Open          at 30 range 0 ..  7;
      Door_4_Open          at 31 range 0 ..  7;
      Door_1_Button        at 32 range 0 ..  7;
      Door_2_Button        at 33 range 0 ..  7;
      Door_3_Button        at 34 range 0 ..  7;
      Door_4_Button        at 35 range 0 ..  7;
      System_Error         at 36 range 0 ..  7;
      System_Time          at 37 range 0 .. 23;
      Sequence_No          at 40 range 0 .. 31;
      Special_Info         at 48 range 0 ..  7;
      Relays               at 49 range 0 ..  7;
      Inputs               at 50 range 0 ..  7;
      System_Date          at 51 range 0 .. 23;
      Padding              at 54 range 0 .. 79;
   end record;

   for Get_Status_Response'Size use 64 * 8;
   for Get_Status_Response'Bit_Order use System.Low_Order_First;
   for Get_Status_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-door reply.
   type Get_Door_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Door;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      OpenDelay  : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 53);
   end record;

   for Get_Door_Response use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Door       at  8 range 0 ..   7;
      Mode       at  9 range 0 ..   7;
      OpenDelay  at 10 range 0 ..   7;
      Padding    at 11 range 0 .. 423;
   end record;

   for Get_Door_Response'Size use 64 * 8;
   for Get_Door_Response'Bit_Order use System.Low_Order_First;
   for Get_Door_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-door reply.
   type Set_Door_Response is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Set_Door;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      OpenDelay  : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 53);
   end record;

   for Set_Door_Response use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Door       at  8 range 0 ..   7;
      Mode       at  9 range 0 ..   7;
      OpenDelay  at 10 range 0 ..   7;
      Padding    at 11 range 0 .. 423;
   end record;

   for Set_Door_Response'Size use 64 * 8;
   for Set_Door_Response'Bit_Order use System.Low_Order_First;
   for Set_Door_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-door-passcodes reply.
   type Set_Door_Passcodes_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Set_Door_Passcodes;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Door_Passcodes_Reply use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Set_Door_Passcodes_Reply'Size use 64 * 8;
   for Set_Door_Passcodes_Reply'Bit_Order use System.Low_Order_First;
   for Set_Door_Passcodes_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an open-door reply.
   type Open_Door_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Open_Door;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Open_Door_Reply use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Open_Door_Reply'Size use 64 * 8;
   for Open_Door_Reply'Bit_Order use System.Low_Order_First;
   for Open_Door_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-cards reply.
   type Get_Cards_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Cards;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Cards      : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52);
   end record;

   for Get_Cards_Reply use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Cards      at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Get_Cards_Reply'Size use 64 * 8;
   for Get_Cards_Reply'Bit_Order use System.Low_Order_First;
   for Get_Cards_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-card reply.
   type Get_Card_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Card;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      Start_Date : Uhppoted.Lib.Types.BCD (1 .. 4);
      End_Date   : Uhppoted.Lib.Types.BCD (1 .. 4);
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_24;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 37);
   end record;

   for Get_Card_Reply use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
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

   for Get_Card_Reply'Size use 64 * 8;
   for Get_Card_Reply'Bit_Order use System.Low_Order_First;
   for Get_Card_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-card-at-index reply.
   type Get_Card_At_Index_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Card_At_Index;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      Start_Date : Uhppoted.Lib.Types.BCD (1 .. 4);
      End_Date   : Uhppoted.Lib.Types.BCD (1 .. 4);
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_24;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 37);
   end record;

   for Get_Card_At_Index_Reply use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
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

   for Get_Card_At_Index_Reply'Size use 64 * 8;
   for Get_Card_At_Index_Reply'Bit_Order use System.Low_Order_First;
   for Get_Card_At_Index_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a put-card reply.
   type Put_Card_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Put_Card;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Put_Card_Reply use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Put_Card_Reply'Size use 64 * 8;
   for Put_Card_Reply'Bit_Order use System.Low_Order_First;
   for Put_Card_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a delete-card reply.
   type Delete_Card_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Delete_Card;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Delete_Card_Reply use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Delete_Card_Reply'Size use 64 * 8;
   for Delete_Card_Reply'Bit_Order use System.Low_Order_First;
   for Delete_Card_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a delete-all-cards reply.
   type Delete_All_Cards_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Delete_All_Cards;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Delete_All_Cards_Reply use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Delete_All_Cards_Reply'Size use 64 * 8;
   for Delete_All_Cards_Reply'Bit_Order use System.Low_Order_First;
   for Delete_All_Cards_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-event reply.
   type Get_Event_Reply is record
      SOM                  : Unsigned_8    := Codec.SOM;
      Opcode               : Codec.Op_Code := Codec.Get_Event;
      Reserved             : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller           : Unsigned_32;
      Index                : Unsigned_32;
      Event_Type           : Unsigned_8;
      Access_Granted       : Unsigned_8;
      Door                 : Unsigned_8;
      Direction            : Unsigned_8;
      Card                 : Unsigned_32;
      Timestamp            : Uhppoted.Lib.Types.BCD (1 .. 7);
      Reason               : Unsigned_8;
      Padding              : Ada.Streams.Stream_Element_Array (1 .. 36);
   end record;

   for Get_Event_Reply use record
      SOM            at  0 range 0 ..   7;
      Opcode         at  1 range 0 ..   7;
      Reserved       at  2 range 0 ..  15;
      Controller     at  4 range 0 ..  31;
      Index          at  8 range 0 ..  31;
      Event_Type     at 12 range 0 ..   7;
      Access_Granted at 13 range 0 ..   7;
      Door           at 14 range 0 ..   7;
      Direction      at 15 range 0 ..   7;
      Card           at 16 range 0 ..  31;
      Timestamp      at 20 range 0 ..  55;
      Reason         at 27 range 0 ..   7;
      Padding        at 28 range 0 .. 287;
   end record;

   for Get_Event_Reply'Size use 64 * 8;
   for Get_Event_Reply'Bit_Order use System.Low_Order_First;
   for Get_Event_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-event-index reply.
   type Get_Event_Index_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Get_Event_Index;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Index      : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52);
   end record;

   for Get_Event_Index_Reply use record
      SOM        at  0 range 0 ..   7;
      Opcode     at  1 range 0 ..   7;
      Reserved   at  2 range 0 ..  15;
      Controller at  4 range 0 ..  31;
      Index      at  8 range 0 ..  31;
      Padding    at 12 range 0 .. 415;
   end record;

   for Get_Event_Index_Reply'Size use 64 * 8;
   for Get_Event_Index_Reply'Bit_Order use System.Low_Order_First;
   for Get_Event_Index_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-event-index reply.
   type Set_Event_Index_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Set_Event_Index;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Event_Index_Reply use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Set_Event_Index_Reply'Size use 64 * 8;
   for Set_Event_Index_Reply'Bit_Order use System.Low_Order_First;
   for Set_Event_Index_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a record-special-events reply.
   type Record_Special_Events_Reply is record
      SOM        : Unsigned_8    := Codec.SOM;
      Opcode     : Codec.Op_Code := Codec.Record_Special_Events;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Record_Special_Events_Reply use record
      SOM        at 0 range 0 ..   7;
      Opcode     at 1 range 0 ..   7;
      Reserved   at 2 range 0 ..  15;
      Controller at 4 range 0 ..  31;
      Ok         at 8 range 0 ..   7;
      Padding    at 9 range 0 .. 439;
   end record;

   for Record_Special_Events_Reply'Size use 64 * 8;
   for Record_Special_Events_Reply'Bit_Order use System.Low_Order_First;
   for Record_Special_Events_Reply'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Replies;
