with AUnit.Assertions;

package body Uhppoted.Lib.Encode.Tests is
   use AUnit.Assertions;
   use Uhppoted.Lib.Types;

   overriding function Name (T : Encoder_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("encoder tests");
   end Name;

   overriding procedure Register_Tests (T : in out Encoder_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Encode_Find_Controllers'Access, "test encode Find_Controllers request");
      Register_Routine (T, Test_Encode_Get_Controller'Access,   "test encode Get_Controller request");
      Register_Routine (T, Test_Encode_Set_IPv4'Access,         "test encode Set_IPv4 request");
      Register_Routine (T, Test_Encode_Get_Time'Access,         "test encode Get_Time request");
      Register_Routine (T, Test_Encode_Set_Time'Access,         "test encode Set_Time request");
      Register_Routine (T, Test_Encode_Get_Listener'Access,     "test encode Get_Listener request");
      Register_Routine (T, Test_Encode_Set_Listener'Access,     "test encode Set_Listener request");
      Register_Routine (T, Test_Encode_Get_Status'Access,       "test encode Get_Status request");
      Register_Routine (T, Test_Encode_Get_Listener_Addr_Port'Access, "test encode Get_Listener_Addr_Port request");
      Register_Routine (T, Test_Encode_Set_Listener_Addrport'Access, "test encode Set_Listener_Addrport request");
      Register_Routine (T, Test_Encode_Get_Door'Access,         "test encode Get_Door request");
      Register_Routine (T, Test_Encode_Set_Door'Access,         "test encode Set_Door request");
      Register_Routine (T, Test_Encode_Set_Door_Passcodes'Access, "test encode Set_Door_Passcodes request");
      Register_Routine (T, Test_Encode_Set_Door_Passcodes_With_Invalid_Passcode'Access, "test encode Set_Door_Passcodes_With_Invalid_Passcode request");
   end Register_Tests;

   procedure Test_Encode_Find_Controllers (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#94#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Controller (
         0);
   begin
      Assert (Request = Expected, "incorrectly encoded get-controller request: got" & Request'Image);
   end Test_Encode_Find_Controllers;

   procedure Test_Encode_Get_Controller (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#94#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Controller (
         405419896);
   begin
      Assert (Request = Expected, "incorrectly encoded get-controller request: got" & Request'Image);
   end Test_Encode_Get_Controller;

   procedure Test_Encode_Set_IPv4 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#96#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#7d#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
         16#c0#, 16#a8#, 16#01#, 16#01#, 16#55#, 16#aa#, 16#aa#, 16#55#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Set_IPv4 (
         405419896,
         Inet_Addr ("192.168.1.125"),
         Inet_Addr ("255.255.255.0"),
         Inet_Addr ("192.168.1.1"));
   begin
      Assert (Request = Expected, "incorrectly encoded set-ipv4 request: got" & Request'Image);
   end Test_Encode_Set_IPv4;

   procedure Test_Encode_Get_Time (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#32#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Time (
         405419896);
   begin
      Assert (Request = Expected, "incorrectly encoded get-time request: got" & Request'Image);
   end Test_Encode_Get_Time;

   procedure Test_Encode_Set_Time (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#30#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#04#, 16#12#, 16#34#, 16#56#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Set_Time (
         405419896,
         (Year => 2025, Month => 11, Day => 4, Hour => 12, Minute => 34, Second => 56));
   begin
      Assert (Request = Expected, "incorrectly encoded set-time request: got" & Request'Image);
   end Test_Encode_Set_Time;

   procedure Test_Encode_Get_Listener (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#92#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Listener (
         405419896);
   begin
      Assert (Request = Expected, "incorrectly encoded get-listener request: got" & Request'Image);
   end Test_Encode_Get_Listener;

   procedure Test_Encode_Set_Listener (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#90#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Set_Listener (
         405419896,
         Inet_Addr ("192.168.1.100"),
         60001,
         17);
   begin
      Assert (Request = Expected, "incorrectly encoded set-listener request: got" & Request'Image);
   end Test_Encode_Set_Listener;

   procedure Test_Encode_Get_Status (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#20#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Status (
         405419896);
   begin
      Assert (Request = Expected, "incorrectly encoded get-status request: got" & Request'Image);
   end Test_Encode_Get_Status;

   procedure Test_Encode_Get_Listener_Addr_Port (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#92#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Listener_Addr_Port (
         405419896);
   begin
      Assert (Request = Expected, "incorrectly encoded get-listener-addr-port request: got" & Request'Image);
   end Test_Encode_Get_Listener_Addr_Port;

   procedure Test_Encode_Set_Listener_Addrport (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#90#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Set_Listener_Addr_Port (
         405419896,
         Network_Socket_Address (Addr => Inet_Addr ("192.168.1.100"), Port => Port_Type (60001)),
         17);
   begin
      Assert (Request = Expected, "incorrectly encoded set-listener-addr:port request: got" & Request'Image);
   end Test_Encode_Set_Listener_Addrport;

   procedure Test_Encode_Get_Door (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#82#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Door (
         405419896,
         3);
   begin
      Assert (Request = Expected, "incorrectly encoded get-door request: got" & Request'Image);
   end Test_Encode_Get_Door;

   procedure Test_Encode_Set_Door (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#80#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#02#, 16#11#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Set_Door (
         405419896,
         3,
         To_Control_Mode (2),
         17);
   begin
      Assert (Request = Expected, "incorrectly encoded set-door request: got" & Request'Image);
   end Test_Encode_Set_Door;

   procedure Test_Encode_Set_Door_Passcodes (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#8c#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#00#, 16#00#, 16#00#, 16#40#, 16#e2#, 16#01#, 16#00#,
         16#47#, 16#94#, 16#03#, 16#00#, 16#4e#, 16#46#, 16#05#, 16#00#,  16#55#, 16#f8#, 16#06#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Set_Door_Passcodes (
         405419896,
         3,
         123456,
         234567,
         345678,
         456789);
   begin
      Assert (Request = Expected, "incorrectly encoded set-door-passcodes request: got" & Request'Image);
   end Test_Encode_Set_Door_Passcodes;

   procedure Test_Encode_Set_Door_Passcodes_With_Invalid_Passcode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#8c#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#00#, 16#00#, 16#00#, 16#40#, 16#e2#, 16#01#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#4e#, 16#46#, 16#05#, 16#00#,  16#55#, 16#f8#, 16#06#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Set_Door_Passcodes (
         405419896,
         3,
         123456,
         1234567,
         345678,
         456789);
   begin
      Assert (Request = Expected, "incorrectly encoded set-door-passcodes request: got" & Request'Image);
   end Test_Encode_Set_Door_Passcodes_With_Invalid_Passcode;

end Uhppoted.Lib.Encode.Tests;
