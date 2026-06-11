with Ada.Streams;
with System;

with Uhppoted.Lib.Types;
with Uhppoted.Lib.Codec;

--  Reply message record definitions.
--

package Uhppoted.Lib.Replies is

   --  (weird gnatdoc bug: blank lines above and below required or param/field tags are rejected)

   --  Convenience type for firmware version.
   --
   --  @field  Major  Major version number.
   --  @field  Minor  Minor version number.
   type Version_Field is record
      Major : Unsigned_8;
      Minor : Unsigned_8;
   end record;

   for Version_Field use
     record
       Major at 0 range 0 .. 7;
       Minor at 0 range 8 .. 15;
     end record;

   for Version_Field'Size use 16;

   --  Message definition for a get-controller reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Address     Controller IPv4 address.
   --  @field  Netmask     IPv4 subnet mask.
   --  @field  Gateway     IPv4 gateway address.
   --  @field  MAC         Controller MAC address.
   --  @field  Version     Firmware version.
   --  @field  Date        Firmware release date.
   --  @field  Padding     Unused bytes.
   type Get_Controller_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
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

   for Get_Controller_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Address at 8 range 0 .. 31;
       Netmask at 12 range 0 .. 31;
       Gateway at 16 range 0 .. 31;
       MAC at 20 range 0 .. 47;
       Version at 26 range 0 .. 15;
       Date at 28 range 0 .. 31;
       Padding at 32 range 0 .. 255;
     end record;

   for Get_Controller_Reply'Size use 64 * 8;
   for Get_Controller_Reply'Bit_Order use System.Low_Order_First;
   for Get_Controller_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-IPv4 reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_IPv4_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_IPv4_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_IPv4_Reply'Size use 64 * 8;
   for Set_IPv4_Reply'Bit_Order use System.Low_Order_First;
   for Set_IPv4_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-time reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Date_Time   Date/time (yyyy-mm-dd HH:mm:ss).
   --  @field  Padding     Unused bytes.
   type Get_Time_Response is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Date_Time  : Uhppoted.Lib.Types.BCD (1 .. 7);
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48);
   end record;

   for Get_Time_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Date_Time at 8 range 0 .. 55;
       Padding at 15 range 0 .. 383;
     end record;

   for Get_Time_Response'Size use 64 * 8;
   for Get_Time_Response'Bit_Order use System.Low_Order_First;
   for Get_Time_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-time reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Date_Time   Date/time (yyyy-mm-dd HH:mm:ss).
   --  @field  Padding     Unused bytes.
   type Set_Time_Response is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Date_Time  : Uhppoted.Lib.Types.BCD (1 .. 7);
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48);
   end record;

   for Set_Time_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Date_Time at 8 range 0 .. 55;
       Padding at 15 range 0 .. 383;
     end record;

   for Set_Time_Response'Size use 64 * 8;
   for Set_Time_Response'Bit_Order use System.Low_Order_First;
   for Set_Time_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-listener reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Address     IPv4 address of event listener.
   --  @field  Port        UDP port [1..65535] of event listener.
   --  @field  Interval    Interval (seconds) at which to send controller state (0 for none).
   --  @field  Padding     Unused bytes.
   type Get_Listener_Response is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Address    : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48);
   end record;

   for Get_Listener_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Address at 8 range 0 .. 31;
       Port at 12 range 0 .. 15;
       Interval at 14 range 0 .. 7;
       Padding at 15 range 0 .. 383;
     end record;

   for Get_Listener_Response'Size use 64 * 8;
   for Get_Listener_Response'Bit_Order use System.Low_Order_First;
   for Get_Listener_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-listener-addrport reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Address     IPv4 address of event listener.
   --  @field  Port        UDP port [1..65535] of event listener.
   --  @field  Interval    Interval (seconds) at which to send controller state (0 for none).
   --  @field  Padding     Unused bytes.
   type Get_Listener_Addr_Port_Response is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Address    : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 48);
   end record;

   for Get_Listener_Addr_Port_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Address at 8 range 0 .. 31;
       Port at 12 range 0 .. 15;
       Interval at 14 range 0 .. 7;
       Padding at 15 range 0 .. 383;
     end record;

   for Get_Listener_Addr_Port_Response'Size use 64 * 8;
   for Get_Listener_Addr_Port_Response'Bit_Order use System.Low_Order_First;
   for Get_Listener_Addr_Port_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-listener reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_Listener_Response is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Boolean;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Listener_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Listener_Response'Size use 64 * 8;
   for Set_Listener_Response'Bit_Order use System.Low_Order_First;
   for Set_Listener_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-listener reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_Listener_Addr_Port_Response is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Boolean;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Listener_Addr_Port_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Listener_Addr_Port_Response'Size use 64 * 8;
   for Set_Listener_Addr_Port_Response'Bit_Order use System.Low_Order_First;
   for Set_Listener_Addr_Port_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-status reply.
   --
   --  @field  SOM                   Start of message byte (16#17#).
   --  @field  OpCode                Packet type/op-code.
   --  @field  Reserved              Unused bytes (reserved for manufacturer use).
   --  @field  Controller            Controller serial number.
   --  @field  Event_Index           Index of most recent event (0 if none).
   --  @field  Event_Type            Event type of most recent event.
   --  @field  Event_Access_Granted  True if most recent event allowed access.
   --  @field  Event_Door            Door ID [1..4] for most recent event.
   --  @field  Event_Direction       Access direction (IN/OUT) for most recent event (0 if none).
   --  @field  Event_Card            Card number for most recent event.
   --  @field  Event_Timestamp       Timestamp of most recent event.
   --  @field  Event_Reason          Reason code of most recent event.
   --  @field  Door_1_Open           True (1) if door 1 sensor is set.
   --  @field  Door_2_Open           True (1) if door 2 sensor is set.
   --  @field  Door_3_Open           True (1) if door 3 sensor is set.
   --  @field  Door_4_Open           True (1) if door 4 sensor is set.
   --  @field  Door_1_Button         True (1) if door 1 push button is pressed.
   --  @field  Door_2_Button         True (1) if door 2 push button is pressed.
   --  @field  Door_3_Button         True (1) if door 3 push button is pressed.
   --  @field  Door_4_Button         True (1) if door 4 push button is pressed.
   --  @field  System_Error          System error code (0 for none).
   --  @field  System_Date           Current system date (yy-mm-dd).
   --  @field  System_Time           Current system time (HH:mm:ss).
   --  @field  Sequence_No           Message sequence number.
   --  @field  Special_Info          Absolutely no idea.
   --  @field  Relays                Door unlock relays bitset.
   --  @field  Inputs                Alarm inputs bitset.
   --  @field  Padding               Unused bytes.
   type Get_Status_Response is record
      SOM                  : Unsigned_8;
      Opcode               : Unsigned_8;
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
      System_Time          : Uhppoted.Lib.Types.BCD (1 .. 3);
      Sequence_No          : Unsigned_32;
      Unused               : Ada.Streams.Stream_Element_Array (1 .. 4);
      Special_Info         : Unsigned_8;
      Relays               : Unsigned_8;
      Inputs               : Unsigned_8;
      System_Date          : Uhppoted.Lib.Types.BCD (1 .. 3);
      Padding              : Ada.Streams.Stream_Element_Array (1 .. 10);
   end record;

   for Get_Status_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Event_Index at 8 range 0 .. 31;
       Event_Type at 12 range 0 .. 7;
       Event_Access_Granted at 13 range 0 .. 7;
       Event_Door at 14 range 0 .. 7;
       Event_Direction at 15 range 0 .. 7;
       Event_Card at 16 range 0 .. 31;
       Event_Timestamp at 20 range 0 .. 55;
       Event_Reason at 27 range 0 .. 7;
       Door_1_Open at 28 range 0 .. 7;
       Door_2_Open at 29 range 0 .. 7;
       Door_3_Open at 30 range 0 .. 7;
       Door_4_Open at 31 range 0 .. 7;
       Door_1_Button at 32 range 0 .. 7;
       Door_2_Button at 33 range 0 .. 7;
       Door_3_Button at 34 range 0 .. 7;
       Door_4_Button at 35 range 0 .. 7;
       System_Error at 36 range 0 .. 7;
       System_Time at 37 range 0 .. 23;
       Sequence_No at 40 range 0 .. 31;
       Unused at 44 range 0 .. 31;
       Special_Info at 48 range 0 .. 7;
       Relays at 49 range 0 .. 7;
       Inputs at 50 range 0 .. 7;
       System_Date at 51 range 0 .. 23;
       Padding at 54 range 0 .. 79;
     end record;

   for Get_Status_Response'Size use 64 * 8;
   for Get_Status_Response'Bit_Order use System.Low_Order_First;
   for Get_Status_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-door reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Door        Door ID [1..4].
   --  @field  Mode        Door control mode (1:normally open, 2:normally closed, 3:controlled).
   --  @field  Open_Delay  Door unlock duration (seconds).
   --  @field  Padding     Unused bytes.
   type Get_Door_Response is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      Open_Delay : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 53);
   end record;

   for Get_Door_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Door at 8 range 0 .. 7;
       Mode at 9 range 0 .. 7;
       Open_Delay at 10 range 0 .. 7;
       Padding at 11 range 0 .. 423;
     end record;

   for Get_Door_Response'Size use 64 * 8;
   for Get_Door_Response'Bit_Order use System.Low_Order_First;
   for Get_Door_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-door reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Door        Door ID [1..4].
   --  @field  Mode        Door control mode (1:normally open, 2:normally closed, 3:controlled).
   --  @field  Open_Delay  Door unlock duration (seconds).
   --  @field  Padding     Unused bytes.
   type Set_Door_Response is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      Open_Delay : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 53);
   end record;

   for Set_Door_Response use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Door at 8 range 0 .. 7;
       Mode at 9 range 0 .. 7;
       Open_Delay at 10 range 0 .. 7;
       Padding at 11 range 0 .. 423;
     end record;

   for Set_Door_Response'Size use 64 * 8;
   for Set_Door_Response'Bit_Order use System.Low_Order_First;
   for Set_Door_Response'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-door-passcodes reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_Door_Passcodes_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Door_Passcodes_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Door_Passcodes_Reply'Size use 64 * 8;
   for Set_Door_Passcodes_Reply'Bit_Order use System.Low_Order_First;
   for Set_Door_Passcodes_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an open-door reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Open_Door_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Open_Door_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Open_Door_Reply'Size use 64 * 8;
   for Open_Door_Reply'Bit_Order use System.Low_Order_First;
   for Open_Door_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-cards reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Cards       Number of valid card records.
   --  @field  Padding     Unused bytes.
   type Get_Cards_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Cards      : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52);
   end record;

   for Get_Cards_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Cards at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Get_Cards_Reply'Size use 64 * 8;
   for Get_Cards_Reply'Bit_Order use System.Low_Order_First;
   for Get_Cards_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-card reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Card        Card number.
   --  @field  Start_Date  Date from which card is valid.
   --  @field  End_Date    Date after which card is no longer valid.
   --  @field  Door_1      Access permissions for door 1 (0:none, 1:24x7, 2..254:time profile).
   --  @field  Door_2      Access permissions for door 2 (0:none, 1:24x7, 2..254:time profile).
   --  @field  Door_3      Access permissions for door 3 (0:none, 1:24x7, 2..254:time profile).
   --  @field  Door_4      Access permissions for door 4 (0:none, 1:24x7, 2..254:time profile).
   --  @field  PIN         Reader keypad PIN [0..999999] (0 for none).
   --  @field  Padding     Unused bytes.
   type Get_Card_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
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

   for Get_Card_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
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

   for Get_Card_Reply'Size use 64 * 8;
   for Get_Card_Reply'Bit_Order use System.Low_Order_First;
   for Get_Card_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-card-at-index reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Card        Card number.
   --  @field  Start_Date  Date from which card is valid.
   --  @field  End_Date    Date after which card is no longer valid.
   --  @field  Door_1      Access permissions for door 1 (0:none, 1:24x7, 2..254:time profile).
   --  @field  Door_2      Access permissions for door 2 (0:none, 1:24x7, 2..254:time profile).
   --  @field  Door_3      Access permissions for door 3 (0:none, 1:24x7, 2..254:time profile).
   --  @field  Door_4      Access permissions for door 4 (0:none, 1:24x7, 2..254:time profile).
   --  @field  PIN         Reader keypad PIN [0..999999] (0 for none).
   --  @field  Padding     Unused bytes.
   type Get_Card_At_Index_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
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

   for Get_Card_At_Index_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
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

   for Get_Card_At_Index_Reply'Size use 64 * 8;
   for Get_Card_At_Index_Reply'Bit_Order use System.Low_Order_First;
   for Get_Card_At_Index_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a put-card reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Put_Card_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Put_Card_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Put_Card_Reply'Size use 64 * 8;
   for Put_Card_Reply'Bit_Order use System.Low_Order_First;
   for Put_Card_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a delete-card reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Delete_Card_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Delete_Card_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Delete_Card_Reply'Size use 64 * 8;
   for Delete_Card_Reply'Bit_Order use System.Low_Order_First;
   for Delete_Card_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a delete-all-cards reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Delete_All_Cards_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Delete_All_Cards_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Delete_All_Cards_Reply'Size use 64 * 8;
   for Delete_All_Cards_Reply'Bit_Order use System.Low_Order_First;
   for Delete_All_Cards_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-event reply.
   --
   --  @field  SOM             Start of message byte (16#17#).
   --  @field  OpCode          Packet type/op-code.
   --  @field  Reserved        Unused bytes (reserved for manufacturer use).
   --  @field  Controller      Controller serial number.
   --  @field  Index           Event record index.
   --  @field  Event_Type      Event type.
   --  @field  Access_Granted  True (1) if event allowed access.
   --  @field  Door            Door ID [1..4].
   --  @field  Direction       Access direction (1:IN, 2: OUT).
   --  @field  Card            Card number.
   --  @field  Timestamp       Event timestamp.
   --  @field  Reason          Event reason code.
   --  @field  Padding         Unused bytes.
   type Get_Event_Reply is record
      SOM            : Unsigned_8;
      Opcode         : Unsigned_8;
      Reserved       : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller     : Unsigned_32;
      Index          : Unsigned_32;
      Event_Type     : Unsigned_8;
      Access_Granted : Unsigned_8;
      Door           : Unsigned_8;
      Direction      : Unsigned_8;
      Card           : Unsigned_32;
      Timestamp      : Uhppoted.Lib.Types.BCD (1 .. 7);
      Reason         : Unsigned_8;
      Padding        : Ada.Streams.Stream_Element_Array (1 .. 36);
   end record;

   for Get_Event_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Index at 8 range 0 .. 31;
       Event_Type at 12 range 0 .. 7;
       Access_Granted at 13 range 0 .. 7;
       Door at 14 range 0 .. 7;
       Direction at 15 range 0 .. 7;
       Card at 16 range 0 .. 31;
       Timestamp at 20 range 0 .. 55;
       Reason at 27 range 0 .. 7;
       Padding at 28 range 0 .. 287;
     end record;

   for Get_Event_Reply'Size use 64 * 8;
   for Get_Event_Reply'Bit_Order use System.Low_Order_First;
   for Get_Event_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-event-index reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Index       Event index.
   --  @field  Padding     Unused bytes.
   type Get_Event_Index_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Index      : Unsigned_32;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 52);
   end record;

   for Get_Event_Index_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Index at 8 range 0 .. 31;
       Padding at 12 range 0 .. 415;
     end record;

   for Get_Event_Index_Reply'Size use 64 * 8;
   for Get_Event_Index_Reply'Bit_Order use System.Low_Order_First;
   for Get_Event_Index_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-event-index reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_Event_Index_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Event_Index_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Event_Index_Reply'Size use 64 * 8;
   for Set_Event_Index_Reply'Bit_Order use System.Low_Order_First;
   for Set_Event_Index_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a record-special-events reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Record_Special_Events_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Record_Special_Events_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Record_Special_Events_Reply'Size use 64 * 8;
   for Record_Special_Events_Reply'Bit_Order use System.Low_Order_First;
   for Record_Special_Events_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-time-profile reply.
   --
   --  @field  SOM              Start of message byte (16#17#).
   --  @field  OpCode           Packet type/op-code.
   --  @field  Reserved         Unused bytes (reserved for manufacturer use).
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
   --  @field  Segment_1_Start  Time of day for start of first time segment.
   --  @field  Segment_1_End    Time of day for end of first time segment.
   --  @field  Segment_2_Start  Time of day for start of second time segment.
   --  @field  Segment_2_End    Time of day for end of second time segment.
   --  @field  Segment_3_Start  Time of day for start of third time segment.
   --  @field  Segment_3_End    Time of day for end of third time segment.
   --  @field  Linked_Profile   Profile ID [2..254] of time profile with additional constraints/segments (0 for none).
   --  @field  Padding          Unused bytes.
   type Get_Time_Profile_Reply is record
      SOM             : Unsigned_8;
      Opcode          : Unsigned_8;
      Reserved        : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller      : Unsigned_32;
      Profile         : Unsigned_8;
      Start_Date      : Uhppoted.Lib.Types.BCD (1 .. 4);
      End_Date        : Uhppoted.Lib.Types.BCD (1 .. 4);
      Monday          : Unsigned_8;
      Tuesday         : Unsigned_8;
      Wednesday       : Unsigned_8;
      Thursday        : Unsigned_8;
      Friday          : Unsigned_8;
      Saturday        : Unsigned_8;
      Sunday          : Unsigned_8;
      Segment_1_Start : Uhppoted.Lib.Types.BCD (1 .. 2);
      Segment_1_End   : Uhppoted.Lib.Types.BCD (1 .. 2);
      Segment_2_Start : Uhppoted.Lib.Types.BCD (1 .. 2);
      Segment_2_End   : Uhppoted.Lib.Types.BCD (1 .. 2);
      Segment_3_Start : Uhppoted.Lib.Types.BCD (1 .. 2);
      Segment_3_End   : Uhppoted.Lib.Types.BCD (1 .. 2);
      Linked_Profile  : Unsigned_8;
      Padding         : Ada.Streams.Stream_Element_Array (1 .. 27);
   end record;

   for Get_Time_Profile_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
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

   for Get_Time_Profile_Reply'Size use 64 * 8;
   for Get_Time_Profile_Reply'Bit_Order use System.Low_Order_First;
   for Get_Time_Profile_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-time-profile reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_Time_Profile_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Time_Profile_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Time_Profile_Reply'Size use 64 * 8;
   for Set_Time_Profile_Reply'Bit_Order use System.Low_Order_First;
   for Set_Time_Profile_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a clear-time-profiles reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Clear_Time_Profiles_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Clear_Time_Profiles_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Clear_Time_Profiles_Reply'Size use 64 * 8;
   for Clear_Time_Profiles_Reply'Bit_Order use System.Low_Order_First;
   for Clear_Time_Profiles_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an add-task reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Add_Task_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Add_Task_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Add_Task_Reply'Size use 64 * 8;
   for Add_Task_Reply'Bit_Order use System.Low_Order_First;
   for Add_Task_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a refresh-tasklist reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Refresh_Task_List_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Refresh_Task_List_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Refresh_Task_List_Reply'Size use 64 * 8;
   for Refresh_Task_List_Reply'Bit_Order use System.Low_Order_First;
   for Refresh_Task_List_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a clear-tasklist reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Clear_Task_List_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Clear_Task_List_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Clear_Task_List_Reply'Size use 64 * 8;
   for Clear_Task_List_Reply'Bit_Order use System.Low_Order_First;
   for Clear_Task_List_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-pc-control reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_PC_Control_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_PC_Control_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_PC_Control_Reply'Size use 64 * 8;
   for Set_PC_Control_Reply'Bit_Order use System.Low_Order_First;
   for Set_PC_Control_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-interlock reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_Interlock_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Interlock_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Interlock_Reply'Size use 64 * 8;
   for Set_Interlock_Reply'Bit_Order use System.Low_Order_First;
   for Set_Interlock_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for an activate-keypads reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Activate_Keypads_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Activate_Keypads_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Activate_Keypads_Reply'Size use 64 * 8;
   for Activate_Keypads_Reply'Bit_Order use System.Low_Order_First;
   for Activate_Keypads_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a get-antipassback reply.
   --
   --  @field  SOM           Start of message byte (16#17#).
   --  @field  OpCode        Packet type/op-code.
   --  @field  Reserved      Unused bytes (reserved for manufacturer use).
   --  @field  Controller    Controller serial number.
   --  @field  Antipassback  Controller anti-passback mode.
   --  @field  Padding       Unused bytes.
   type Get_Antipassback_Reply is record
      SOM          : Unsigned_8;
      Opcode       : Unsigned_8;
      Reserved     : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller   : Unsigned_32;
      Antipassback : Unsigned_8;
      Padding      : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Get_Antipassback_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Antipassback at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Get_Antipassback_Reply'Size use 64 * 8;
   for Get_Antipassback_Reply'Bit_Order use System.Low_Order_First;
   for Get_Antipassback_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-antipassback reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_Antipassback_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_Antipassback_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_Antipassback_Reply'Size use 64 * 8;
   for Set_Antipassback_Reply'Bit_Order use System.Low_Order_First;
   for Set_Antipassback_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a set-firstcard reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Set_First_Card_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Set_First_Card_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Set_First_Card_Reply'Size use 64 * 8;
   for Set_First_Card_Reply'Bit_Order use System.Low_Order_First;
   for Set_First_Card_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a restore-default-parameters reply.
   --
   --  @field  SOM         Start of message byte (16#17#).
   --  @field  OpCode      Packet type/op-code.
   --  @field  Reserved    Unused bytes (reserved for manufacturer use).
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success (1) or fail (0) result.
   --  @field  Padding     Unused bytes.
   type Restore_Default_Parameters_Reply is record
      SOM        : Unsigned_8;
      Opcode     : Unsigned_8;
      Reserved   : Ada.Streams.Stream_Element_Array (1 .. 2) := [others => 0];
      Controller : Unsigned_32;
      Ok         : Unsigned_8;
      Padding    : Ada.Streams.Stream_Element_Array (1 .. 55);
   end record;

   for Restore_Default_Parameters_Reply use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Ok at 8 range 0 .. 7;
       Padding at 9 range 0 .. 439;
     end record;

   for Restore_Default_Parameters_Reply'Size use 64 * 8;
   for Restore_Default_Parameters_Reply'Bit_Order use System.Low_Order_First;
   for Restore_Default_Parameters_Reply'Scalar_Storage_Order use System.Low_Order_First;

   --  Message definition for a listener-event message.
   --
   --  @field  SOM                   Start of message byte (16#17#).
   --  @field  OpCode                Packet type/op-code.
   --  @field  Reserved              Unused bytes (reserved for manufacturer use).
   --  @field  Controller            Controller serial number.
   --  @field  Event_Index           Index of most recent event (0 if none).
   --  @field  Event_Type            Event type of most recent event.
   --  @field  Event_Access_Granted  True if most recent event allowed access.
   --  @field  Event_Door            Door ID [1..4] for most recent event.
   --  @field  Event_Direction       Access direction (IN/OUT) for most recent event (0 if none).
   --  @field  Event_Card            Card number for most recent event.
   --  @field  Event_Timestamp       Timestamp of most recent event.
   --  @field  Event_Reason          Reason code of most recent event.
   --  @field  Door_1_Open           True (1) if door 1 sensor is set.
   --  @field  Door_2_Open           True (1) if door 2 sensor is set.
   --  @field  Door_3_Open           True (1) if door 3 sensor is set.
   --  @field  Door_4_Open           True (1) if door 4 sensor is set.
   --  @field  Door_1_Button         True (1) if door 1 push button is pressed.
   --  @field  Door_2_Button         True (1) if door 2 push button is pressed.
   --  @field  Door_3_Button         True (1) if door 3 push button is pressed.
   --  @field  Door_4_Button         True (1) if door 4 push button is pressed.
   --  @field  System_Error          System error code (0 for none).
   --  @field  System_Date           Current system date (yy-mm-dd).
   --  @field  System_Time           Current system time (HH:mm:ss).
   --  @field  Sequence_No           Message sequence number.
   --  @field  Special_Info          Absolutely no idea.
   --  @field  Relays                Door unlock relays bitset.
   --  @field  Inputs                Alarm inputs bitset.
   --  @field  Padding               Unused bytes.
   type Listener_Event is record
      SOM                  : Unsigned_8 := Codec.SOM;
      Opcode               : Unsigned_8 := 16#20#;
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
      System_Time          : Uhppoted.Lib.Types.BCD (1 .. 3);
      Sequence_No          : Unsigned_32;
      Unused               : Ada.Streams.Stream_Element_Array (1 .. 4);
      Special_Info         : Unsigned_8;
      Relays               : Unsigned_8;
      Inputs               : Unsigned_8;
      System_Date          : Uhppoted.Lib.Types.BCD (1 .. 3);
      Padding              : Ada.Streams.Stream_Element_Array (1 .. 10);
   end record;

   for Listener_Event use
     record
       SOM at 0 range 0 .. 7;
       Opcode at 1 range 0 .. 7;
       Reserved at 2 range 0 .. 15;
       Controller at 4 range 0 .. 31;
       Event_Index at 8 range 0 .. 31;
       Event_Type at 12 range 0 .. 7;
       Event_Access_Granted at 13 range 0 .. 7;
       Event_Door at 14 range 0 .. 7;
       Event_Direction at 15 range 0 .. 7;
       Event_Card at 16 range 0 .. 31;
       Event_Timestamp at 20 range 0 .. 55;
       Event_Reason at 27 range 0 .. 7;
       Door_1_Open at 28 range 0 .. 7;
       Door_2_Open at 29 range 0 .. 7;
       Door_3_Open at 30 range 0 .. 7;
       Door_4_Open at 31 range 0 .. 7;
       Door_1_Button at 32 range 0 .. 7;
       Door_2_Button at 33 range 0 .. 7;
       Door_3_Button at 34 range 0 .. 7;
       Door_4_Button at 35 range 0 .. 7;
       System_Error at 36 range 0 .. 7;
       System_Time at 37 range 0 .. 23;
       Sequence_No at 40 range 0 .. 31;
       Unused at 44 range 0 .. 31;
       Special_Info at 48 range 0 .. 7;
       Relays at 49 range 0 .. 7;
       Inputs at 50 range 0 .. 7;
       System_Date at 51 range 0 .. 23;
       Padding at 54 range 0 .. 79;
     end record;

   for Listener_Event'Size use 64 * 8;
   for Listener_Event'Bit_Order use System.Low_Order_First;
   for Listener_Event'Scalar_Storage_Order use System.Low_Order_First;

end Uhppoted.Lib.Replies;
