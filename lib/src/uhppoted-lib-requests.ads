with Ada.Streams;
with System;
with Uhppoted.Lib.Codec;

--  Request message record definitions.
--

package Uhppoted.Lib.Requests is

   --  BCD HHmm type.
   type BCD2 is array (1 .. 2) of Unsigned_8;

   --  BCD date type.
   type BCD4 is array (1 .. 4) of Unsigned_8;

   --  BCD date/time type.
   type BCD7 is array (1 .. 7) of Unsigned_8;

   --  Message definition for a get-controller request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   type Get_Controller_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Controller;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Controller_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Padding at 8 range 0 .. 447;
     end record;

   for Get_Controller_Request'Size use 64 * 8;
   for Get_Controller_Request'Bit_Order use System.Low_Order_First;
   for Get_Controller_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-IPv4 request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Addr        Controller IPv4 address.
   --  @field  Netmask     Controller IPv4 subnet mask.
   --  @field  Gateway     Controller IPv4 gateway address.
   --  @field  MagicWord   Hard-coded authorisation constant.
   type Set_IPv4_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_IPv4;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Addr       : IPv4;
      Netmask    : IPv4;
      Gateway    : IPv4;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 40) := [others => 0];
   end record;

   for Set_IPv4_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Addr at 8 range 0 .. 31;
       Netmask at 12 range 0 .. 31;
       Gateway at 16 range 0 .. 31;
       MagicWord at 20 range 0 .. 31;
       Padding at 24 range 0 .. 319;
     end record;

   for Set_IPv4_Request'Size use 64 * 8;
   for Set_IPv4_Request'Bit_Order use System.Low_Order_First;
   for Set_IPv4_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-time request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   type Get_Time_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Time;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Time_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Padding at 8 range 0 .. 447;
     end record;

   for Get_Time_Request'Size use 64 * 8;
   for Get_Time_Request'Bit_Order use System.Low_Order_First;
   for Get_Time_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-time request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Date_Time   Date/time (yyyy-mm-dd HH:mm).
   type Set_Time_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Time;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Date_Time  : BCD7 := [others => 0];
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 49) := [others => 0];
   end record;

   for Set_Time_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Date_Time at 8 range 0 .. 55;
       Padding at 15 range 0 .. 391;
     end record;

   for Set_Time_Request'Size use 64 * 8;
   for Set_Time_Request'Bit_Order use System.Low_Order_First;
   for Set_Time_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-listener request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   type Get_Listener_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Listener;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Listener_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Padding at 8 range 0 .. 447;
     end record;

   for Get_Listener_Request'Size use 64 * 8;
   for Get_Listener_Request'Bit_Order use System.Low_Order_First;
   for Get_Listener_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-listener request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Addr        Listener IPv4 address.
   --  @field  Port        Listener port [1..65535].
   --  @field  Interval    Interval (seconds) at which to automatically send controller state (0 for none).
   type Set_Listener_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Listener;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Addr       : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 49) := [others => 0];
   end record;

   for Set_Listener_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Addr at 8 range 0 .. 31;
       Port at 12 range 0 .. 15;
       Interval at 14 range 0 .. 7;
       Padding at 15 range 0 .. 391;
     end record;

   for Set_Listener_Request'Size use 64 * 8;
   for Set_Listener_Request'Bit_Order use System.Low_Order_First;
   for Set_Listener_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-status request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   type Get_Status_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Status;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Status_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Padding at 8 range 0 .. 447;
     end record;

   for Get_Status_Request'Size use 64 * 8;
   for Get_Status_Request'Bit_Order use System.Low_Order_First;
   for Get_Status_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-door request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Door        Door ID [1..4].
   type Get_Door_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Door;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Get_Door_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Door at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Get_Door_Request'Size use 64 * 8;
   for Get_Door_Request'Bit_Order use System.Low_Order_First;
   for Get_Door_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-door request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Door        Door ID [1..4].
   --  @field  Mode        Door control mode (1:normally open, 2:normally close, 3: controlled).
   --  @field  OpenDelay   Door unlock duration (seconds).
   type Set_Door_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Door;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Control_Mode;
      OpenDelay  : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 53) := [others => 0];
   end record;

   for Set_Door_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Door at 8 range 0 .. 7;
       Mode at 9 range 0 .. 7;
       OpenDelay at 10 range 0 .. 7;
       Padding at 11 range 0 .. 423;
     end record;

   for Set_Door_Request'Size use 64 * 8;
   for Set_Door_Request'Bit_Order use System.Low_Order_First;
   for Set_Door_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-door-passcodes request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Door        Door ID [1..4].
   --  @field  Passcode1   First passcode [0..999999] (0 for none).
   --  @field  Passcode2   Second passcode [0..999999] (0 for none).
   --  @field  Passcode3   Third passcode [0..999999] (0 for none).
   --  @field  Passcode4   Fourth passcode [0..999999] (0 for none).
   type Set_Door_Passcodes_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Door_Passcodes;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      --  @exclude
      Reserved2  : Ada.Streams.Stream_Element_Array (1 .. 3) := [others => 0];
      Passcode1  : Unsigned_32 := 0;
      Passcode2  : Unsigned_32 := 0;
      Passcode3  : Unsigned_32 := 0;
      Passcode4  : Unsigned_32 := 0;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 36) := [others => 0];
   end record;

   for Set_Door_Passcodes_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Door at 8 range 0 .. 7;
       Reserved2 at 9 range 0 .. 23;
       Passcode1 at 12 range 0 .. 31;
       Passcode2 at 16 range 0 .. 31;
       Passcode3 at 20 range 0 .. 31;
       Passcode4 at 24 range 0 .. 31;
       Padding at 28 range 0 .. 287;
     end record;

   for Set_Door_Passcodes_Request'Size use 64 * 8;
   for Set_Door_Passcodes_Request'Bit_Order use System.Low_Order_First;
   for Set_Door_Passcodes_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an open-door request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Door        Door ID [1..4].
   type Open_Door_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Open_Door;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Open_Door_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Door at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Open_Door_Request'Size use 64 * 8;
   for Open_Door_Request'Bit_Order use System.Low_Order_First;
   for Open_Door_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-cards request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   type Get_Cards_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Cards;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Cards_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Padding at 8 range 0 .. 447;
     end record;

   for Get_Cards_Request'Size use 64 * 8;
   for Get_Cards_Request'Bit_Order use System.Low_Order_First;
   for Get_Cards_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-card request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Card        Card number.
   type Get_Card_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Card;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Get_Card_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Card at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Get_Card_Request'Size use 64 * 8;
   for Get_Card_Request'Bit_Order use System.Low_Order_First;
   for Get_Card_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-card-by-index request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Index       Card record index.
   type Get_Card_At_Index_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Card_At_Index;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Index      : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Get_Card_At_Index_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Index at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Get_Card_At_Index_Request'Size use 64 * 8;
   for Get_Card_At_Index_Request'Bit_Order use System.Low_Order_First;
   for Get_Card_At_Index_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a put-card request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Card        Card number.
   --  @field  Start_Date  Date from which card is valid.
   --  @field  End_Date    Date after which card is no longer valid.
   --  @field  Door_1      Access permissions for door 1 (0: none, 1:24x7, 2..254: time profile).
   --  @field  Door_2      Access permissions for door 2 (0: none, 1:24x7, 2..254: time profile).
   --  @field  Door_3      Access permissions for door 3 (0: none, 1:24x7, 2..254: time profile).
   --  @field  Door_4      Access permissions for door 4 (0: none, 1:24x7, 2..254: time profile).
   --  @field  PIN         Access reader PIN code [0..999999] (0 for none).
   type Put_Card_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Put_Card;
      --  @exclude
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
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 37) := [others => 0];
   end record;

   for Put_Card_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Card at 8 range 0 .. 31;
       Start_Date at 12 range 0 .. 31;
       End_Date at 16 range 0 .. 31;
       Door_1 at 20 range 0 .. 7;
       Door_2 at 21 range 0 .. 7;
       Door_3 at 22 range 0 .. 7;
       Door_4 at 23 range 0 .. 7;
       PIN at 24 range 0 .. 23;
       Padding at 27 range 0 .. 295;
     end record;

   for Put_Card_Request'Size use 64 * 8;
   for Put_Card_Request'Bit_Order use System.Low_Order_First;
   for Put_Card_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a delete-card request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Card        Card number.
   type Delete_Card_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Delete_Card;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Delete_Card_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Card at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Delete_Card_Request'Size use 64 * 8;
   for Delete_Card_Request'Bit_Order use System.Low_Order_First;
   for Delete_Card_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a delete-cards request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  MagicWord   Hard-coded authorisation constant.
   type Delete_Cards_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Delete_All_Cards;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Delete_Cards_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       MagicWord at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Delete_Cards_Request'Size use 64 * 8;
   for Delete_Cards_Request'Bit_Order use System.Low_Order_First;
   for Delete_Cards_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-event request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Index       Event index.
   type Get_Event_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Event;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Index      : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Get_Event_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Index at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Get_Event_Request'Size use 64 * 8;
   for Get_Event_Request'Bit_Order use System.Low_Order_First;
   for Get_Event_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-event-index request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   type Get_Event_Index_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Event_Index;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 53) := [others => 0];
   end record;

   for Get_Event_Index_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Padding at 8 range 0 .. 423;
     end record;

   for Get_Event_Index_Request'Size use 64 * 8;
   for Get_Event_Index_Request'Bit_Order use System.Low_Order_First;
   for Get_Event_Index_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-event-index request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Index       Event index.
   --  @field  MagicWord   Hard-coded authorisation constant.
   type Set_Event_Index_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Event_Index;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Index      : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48) := [others => 0];
   end record;

   for Set_Event_Index_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Index at 8 range 0 .. 31;
       MagicWord at 12 range 0 .. 31;
       Padding at 16 range 0 .. 383;
     end record;

   for Set_Event_Index_Request'Size use 64 * 8;
   for Set_Event_Index_Request'Bit_Order use System.Low_Order_First;
   for Set_Event_Index_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a record-special-events request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Enabled     Enables/disables door/pusbutton/etc events.
   type Record_Special_Events_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Record_Special_Events;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Enabled    : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Record_Special_Events_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Enabled at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Record_Special_Events_Request'Size use 64 * 8;
   for Record_Special_Events_Request'Bit_Order use System.Low_Order_First;
   for Record_Special_Events_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-time-profile request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Profile     Time profile ID [2..254].
   type Get_Time_Profile_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Time_Profile;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Profile    : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Get_Time_Profile_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Profile at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Get_Time_Profile_Request'Size use 64 * 8;
   for Get_Time_Profile_Request'Bit_Order use System.Low_Order_First;
   for Get_Time_Profile_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-time-profile request.
   --
   --  @field  SOM              Start of message byte (16#17).
   --  @field  OpCode           Message type/op-code.
   --  @field  Controller       Controller serial number.
   --  @field  Profile          Time profile ID [2..254].
   --  @field  Start_Date       Date from which time profile is enabled.
   --  @field  End_Date         Date after which time profile is no longer enabled.
   --  @field  Monday           Enables/disables time profile on Monday.
   --  @field  Tuesday          Enables/disables time profile on Tuesday.
   --  @field  Wednesday        Enables/disables time profile on Wednesday.
   --  @field  Thursday         Enables/disables time profile on Thursday.
   --  @field  Friday           Enables/disables time profile on Friday.
   --  @field  Saturday         Enables/disables time profile on Saturday.
   --  @field  Sunday           Enables/disables time profile on Sunday.
   --  @field  Segment_1_Start  Hour of day for start of first time segment.
   --  @field  Segment_1_End    Hour of day for end of first time segment.
   --  @field  Segment_2_Start  Hour of day for start of second time segment.
   --  @field  Segment_2_End    Hour of day for end of second time segment.
   --  @field  Segment_3_Start  Hour of day for start of third time segment.
   --  @field  Segment_3_End    Hour of day for end of third time segment.
   --  @field  Linked_Profile   Profile ID [2..254] of time profile with additional
   --                           constraints/segments (0 if none).
   type Set_Time_Profile_Request is record
      SOM             : Unsigned_8 := Codec.SOM;
      OpCode          : Codec.Op_Code := Codec.Set_Time_Profile;
      --  @exclude
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
      --  @exclude
      Padding         : Ada.Streams.Stream_Element_Array (1 .. 27) := [others => 0];
   end record;

   for Set_Time_Profile_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Profile at 8 range 0 .. 7;
       Start_Date at 9 range 0 .. 31;
       End_Date at 13 range 0 .. 31;
       Monday at 17 range 0 .. 7;
       Tuesday at 18 range 0 .. 7;
       Wednesday at 19 range 0 .. 7;
       Thursday at 20 range 0 .. 7;
       Friday at 21 range 0 .. 7;
       Saturday at 22 range 0 .. 7;
       Sunday at 23 range 0 .. 7;
       Segment_1_Start at 24 range 0 .. 15;
       Segment_1_End at 26 range 0 .. 15;
       Segment_2_Start at 28 range 0 .. 15;
       Segment_2_End at 30 range 0 .. 15;
       Segment_3_Start at 32 range 0 .. 15;
       Segment_3_End at 34 range 0 .. 15;
       Linked_Profile at 36 range 0 .. 7;
       Padding at 37 range 0 .. 215;
     end record;

   for Set_Time_Profile_Request'Size use 64 * 8;
   for Set_Time_Profile_Request'Bit_Order use System.Low_Order_First;
   for Set_Time_Profile_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a clear-time-profiles request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  MagicWord   Hard-coded authorisation constant.
   type Clear_Time_Profiles_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Clear_Time_Profiles;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Clear_Time_Profiles_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       MagicWord at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Clear_Time_Profiles_Request'Size use 64 * 8;
   for Clear_Time_Profiles_Request'Bit_Order use System.Low_Order_First;
   for Clear_Time_Profiles_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an add-task request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Start_Date  Date from which scheduled task is enabled.
   --  @field  End_Date    Date after which scheduled task is no longer enabled.
   --  @field  Monday      Enables/disables scheduled task on Monday.
   --  @field  Tuesday     Enables/disables scheduled task on Tuesday.
   --  @field  Wednesday   Enables/disables scheduled task on Wednesday.
   --  @field  Thursday    Enables/disables scheduled task on Thursday.
   --  @field  Friday      Enables/disables scheduled task on Friday.
   --  @field  Saturday    Enables/disables scheduled task on Saturday.
   --  @field  Sunday      Enables/disables scheduled task on Sunday.
   --  @field  Start_Time  Hour of day at which scheduled task is run.
   --  @field  Door        Door ID [1..4] for scheduled task action.
   --  @field  Task_ID     Scheduled task type.
   --  @field  More_Cards  Number of 'more cards' for More_Cards task ID.
   type Add_Task_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Add_Task;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Start_Date : BCD4;
      End_Date   : BCD4;
      Monday     : Unsigned_8;
      Tuesday    : Unsigned_8;
      Wednesday  : Unsigned_8;
      Thursday   : Unsigned_8;
      Friday     : Unsigned_8;
      Saturday   : Unsigned_8;
      Sunday     : Unsigned_8;
      Start_Time : BCD2;
      Door       : Unsigned_8;
      Task_ID    : Task_Type;
      More_Cards : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 35) := [others => 0];
   end record;

   for Add_Task_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Start_Date at 8 range 0 .. 31;
       End_Date at 12 range 0 .. 31;
       Monday at 16 range 0 .. 7;
       Tuesday at 17 range 0 .. 7;
       Wednesday at 18 range 0 .. 7;
       Thursday at 19 range 0 .. 7;
       Friday at 20 range 0 .. 7;
       Saturday at 21 range 0 .. 7;
       Sunday at 22 range 0 .. 7;
       Start_Time at 23 range 0 .. 15;
       Door at 25 range 0 .. 7;
       Task_ID at 26 range 0 .. 7;
       More_Cards at 27 range 0 .. 7;
       Padding at 28 range 0 .. 279;
     end record;

   for Add_Task_Request'Size use 64 * 8;
   for Add_Task_Request'Bit_Order use System.Low_Order_First;
   for Add_Task_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a refresh-tasklist request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  MagicWord   Hard-coded authorisation constant.
   type Refresh_Task_List_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Refresh_Task_List;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Refresh_Task_List_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       MagicWord at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Refresh_Task_List_Request'Size use 64 * 8;
   for Refresh_Task_List_Request'Bit_Order use System.Low_Order_First;
   for Refresh_Task_List_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a clear-tasklist request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  MagicWord   Hard-coded authorisation constant.
   type Clear_Task_List_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Clear_Task_List;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Clear_Task_List_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       MagicWord at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Clear_Task_List_Request'Size use 64 * 8;
   for Clear_Task_List_Request'Bit_Order use System.Low_Order_First;
   for Clear_Task_List_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-pc-control request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  MagicWord   Hard-coded authorisation constant.
   --  @field  Enable      Enables/disables remote access control.
   type Set_PC_Control_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_PC_Control;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      Enable     : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 51) := [others => 0];
   end record;

   for Set_PC_Control_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       MagicWord at 8 range 0 .. 31;
       Enable at 12 range 0 .. 7;
       Padding at 13 range 0 .. 407;
     end record;

   for Set_PC_Control_Request'Size use 64 * 8;
   for Set_PC_Control_Request'Bit_Order use System.Low_Order_First;
   for Set_PC_Control_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-interlock request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Interlock   Door interlock mode.
   type Set_Interlock_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Set_Interlock;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Interlock  : Uhppoted.Lib.Interlock;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Set_Interlock_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Interlock at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Interlock_Request'Size use 64 * 8;
   for Set_Interlock_Request'Bit_Order use System.Low_Order_First;
   for Set_Interlock_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an activate-keypads request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  Reader_1    Enables/disables keypads for reader 1.
   --  @field  Reader_2    Enables/disables keypads for reader 2.
   --  @field  Reader_3    Enables/disables keypads for reader 3.
   --  @field  Reader_4    Enables/disables keypads for reader 4.
   type Activate_Keypads_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Activate_Keypads;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Reader_1   : Unsigned_8;
      Reader_2   : Unsigned_8;
      Reader_3   : Unsigned_8;
      Reader_4   : Unsigned_8;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Activate_Keypads_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Reader_1 at 8 range 0 .. 7;
       Reader_2 at 9 range 0 .. 7;
       Reader_3 at 10 range 0 .. 7;
       Reader_4 at 11 range 0 .. 7;
       Padding at 12 range 0 .. 415;
     end record;

   for Activate_Keypads_Request'Size use 64 * 8;
   for Activate_Keypads_Request'Bit_Order use System.Low_Order_First;
   for Activate_Keypads_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-antipassback request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   type Get_Antipassback_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Get_Antipassback;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 56) := [others => 0];
   end record;

   for Get_Antipassback_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Padding at 8 range 0 .. 447;
     end record;

   for Get_Antipassback_Request'Size use 64 * 8;
   for Get_Antipassback_Request'Bit_Order use System.Low_Order_First;
   for Get_Antipassback_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-antipassback request.
   --
   --  @field  SOM           Start of message byte (16#17).
   --  @field  OpCode        Message type/op-code.
   --  @field  Controller    Controller serial number.
   --  @field  Antipassback  Anti-passback mode.
   type Set_Antipassback_Request is record
      SOM          : Unsigned_8 := Codec.SOM;
      OpCode       : Codec.Op_Code := Codec.Set_Antipassback;
      --  @exclude
      Reserved     : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller   : Unsigned_32;
      Antipassback : Uhppoted.Lib.Antipassback;
      --  @exclude
      Padding      : Ada.Streams.Stream_Element_Array (1 .. 55) := [others => 0];
   end record;

   for Set_Antipassback_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Antipassback at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Antipassback_Request'Size use 64 * 8;
   for Set_Antipassback_Request'Bit_Order use System.Low_Order_First;
   for Set_Antipassback_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-firstcard request.
   --
   --  @field  SOM            Start of message byte (16#17).
   --  @field  OpCode         Message type/op-code.
   --  @field  Controller     Controller serial number.
   --  @field  Door           Door ID [1..4].
   --  @field  Start_Time     Time of day from which first-card mode can be activated.
   --  @field  End_Time       Time of day after which first-card mode is deactivated.
   --  @field  Active_Mode    Door control mode after a first-card swipe activates first-card mode.
   --  @field  Inactive_Mode  Door control mode when first-card mode is not activated.
   --  @field  Monday         Enables/disables first-card mode on Monday.
   --  @field  Tuesday        Enables/disables first-card mode on Tuesday.
   --  @field  Wednesday      Enables/disables first-card mode on Wednesday.
   --  @field  Thursday       Enables/disables first-card mode on Thursday.
   --  @field  Friday         Enables/disables first-card mode on Friday.
   --  @field  Saturday       Enables/disables first-card mode on Saturday.
   --  @field  Sunday         Enables/disables first-card mode on Sunday.
   type Set_First_Card_Request is record
      SOM           : Unsigned_8 := Codec.SOM;
      OpCode        : Codec.Op_Code := Codec.Set_First_Card;
      --  @exclude
      Reserved      : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller    : Unsigned_32;
      Door          : Unsigned_8;
      Start_Time    : BCD2;
      Active_Mode   : Unsigned_8;
      End_Time      : BCD2;
      Inactive_Mode : Unsigned_8;
      Monday        : Unsigned_8;
      Tuesday       : Unsigned_8;
      Wednesday     : Unsigned_8;
      Thursday      : Unsigned_8;
      Friday        : Unsigned_8;
      Saturday      : Unsigned_8;
      Sunday        : Unsigned_8;
      --  @exclude
      Padding       : Ada.Streams.Stream_Element_Array (1 .. 42) := [others => 0];
   end record;

   for Set_First_Card_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Door at 8 range 0 .. 7;
       Start_Time at 9 range 0 .. 15;
       Active_Mode at 11 range 0 .. 7;
       End_Time at 12 range 0 .. 15;
       Inactive_Mode at 14 range 0 .. 7;
       Monday at 15 range 0 .. 7;
       Tuesday at 16 range 0 .. 7;
       Wednesday at 17 range 0 .. 7;
       Thursday at 18 range 0 .. 7;
       Friday at 19 range 0 .. 7;
       Saturday at 20 range 0 .. 7;
       Sunday at 21 range 0 .. 7;
       Padding at 22 range 0 .. 335;
     end record;

   for Set_First_Card_Request'Size use 64 * 8;
   for Set_First_Card_Request'Bit_Order use System.Low_Order_First;
   for Set_First_Card_Request'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a restore-default-parameters request.
   --
   --  @field  SOM         Start of message byte (16#17).
   --  @field  OpCode      Message type/op-code.
   --  @field  Controller  Controller serial number.
   --  @field  MagicWord   Hard-coded authorisation constant.
   type Restore_Default_Parameters_Request is record
      SOM        : Unsigned_8 := Codec.SOM;
      OpCode     : Codec.Op_Code := Codec.Restore_Default_Parameters;
      --  @exclude
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      MagicWord  : Unsigned_32 := Codec.MagicWord;
      --  @exclude
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52) := [others => 0];
   end record;

   for Restore_Default_Parameters_Request use
     record
       SOM at 0 range 0 .. 7;
       OpCode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       MagicWord at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Restore_Default_Parameters_Request'Size use 64 * 8;
   for Restore_Default_Parameters_Request'Bit_Order use System.Low_Order_First;
   for Restore_Default_Parameters_Request'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Requests;
