with AUnit.Assertions;
with Ada.Strings.Unbounded;

package body Uhppoted.Lib.Decode.Tests is
   use AUnit.Assertions;
   use Ada.Strings.Unbounded;

   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   overriding function Name (T : Decoder_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("decoder tests");
   end Name;

   overriding procedure Register_Tests (T : in out Decoder_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Decode_Get_Controller'Access, "test decode Get_Controller response");
      Register_Routine (T, Test_Decode_Set_IPv4'Access,     "test decode Set_IPv4 response");
      Register_Routine (T, Test_Decode_Get_Time'Access,     "test decode Get_Time response");
      Register_Routine (T, Test_Decode_Set_Time'Access,     "test decode Set_Time response");
      Register_Routine (T, Test_Decode_Get_Listener'Access, "test decode Get_Listener response");
      Register_Routine (T, Test_Decode_Set_Listener'Access, "test decode Set_Listener response");
      Register_Routine (T, Test_Decode_Get_Status'Access,   "test decode Get_Status response");
      Register_Routine (T, Test_Decode_Get_Listener_Address_Port'Access, "test decode Get_Listener_Address_Port response");
      Register_Routine (T, Test_Decode_Set_Listener_Address_Port'Access, "test decode Set_Listener_Address_Port response");
      Register_Routine (T, Test_Decode_Get_Door'Access,     "test decode Get_Door response");
      Register_Routine (T, Test_Decode_Set_Door'Access,     "test decode Set_Door response");
      Register_Routine (T, Test_Decode_Set_Door_Passcodes'Access, "test decode Set_Door_Passcodes response");
      Register_Routine (T, Test_Decode_Open_Door'Access,    "test decode Open_Door response");
      Register_Routine (T, Test_Decode_Get_Cards'Access,    "test decode Get_Cards response");
      Register_Routine (T, Test_Decode_Get_Card'Access,     "test decode Get_Card response");
      Register_Routine (T, Test_Decode_Get_Card_Not_Found'Access, "test decode Get_Card_Not_Found response");
      Register_Routine (T, Test_Decode_Get_Card_At_Index'Access, "test decode Get_Card_At_Index response");
      Register_Routine (T, Test_Decode_Put_Card'Access,     "test decode Put_Card response");
      Register_Routine (T, Test_Decode_Delete_Card'Access,  "test decode Delete_Card response");
      Register_Routine (T, Test_Decode_Delete_All_Cards'Access, "test decode Delete_All_Cards response");
      Register_Routine (T, Test_Decode_Get_Event_Index'Access, "test decode Get_Event_Index response");
      Register_Routine (T, Test_Decode_Set_Event_Index'Access, "test decode Set_Event_Index response");
      Register_Routine (T, Test_Decode_Record_Special_Events'Access, "test decode Record_Special_Events response");
   end Register_Tests;

   procedure Test_Decode_Get_Controller (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Controller_Response := (
         Controller  => 405419896,
         IP_Address  => [192, 168, 1, 100],
         Subnet_Mask => [255, 255, 255, 0],
         Gateway     => [192, 168, 1, 1],
         MAC_Address => [16#00#, 16#12#, 16#23#, 16#34#, 16#45#, 16#56#],
         Version     => To_Unbounded_String ("v8.92"),
         Date        => (Year => 2018, Month => 11, Day => 5));

      Reply : constant Packet := [
         16#17#, 16#94#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
         16#c0#, 16#a8#, 16#01#, 16#01#, 16#00#, 16#12#, 16#23#, 16#34#,  16#45#, 16#56#, 16#08#, 16#92#, 16#20#, 16#18#, 16#11#, 16#05#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Controller_Response := Uhppoted.Lib.Decode.Get_Controller (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-controller response: got" & Response'Image);
   end Test_Decode_Get_Controller;

   procedure Test_Decode_Set_IPv4 (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Set_IPv4_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#96#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Set_IPv4_Response := Uhppoted.Lib.Decode.Set_IPv4 (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded set-ipv4 response: got" & Response'Image);
   end Test_Decode_Set_IPv4;

   procedure Test_Decode_Get_Time (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Time_Response := (
         Controller => 405419896,
         Date_Time  => (Year => 2025, Month => 11, Day => 1, Hour => 12, Minute => 34, Second => 56));

      Reply : constant Packet := [
         16#17#, 16#32#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Time_Response := Uhppoted.Lib.Decode.Get_Time (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-time response: got" & Response'Image);
   end Test_Decode_Get_Time;

   procedure Test_Decode_Set_Time (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Set_Time_Response := (
         Controller => 405419896,
         Date_Time  => (Year => 2025, Month => 11, Day => 1, Hour => 12, Minute => 34, Second => 56));

      Reply : constant Packet := [
         16#17#, 16#30#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Set_Time_Response := Uhppoted.Lib.Decode.Set_Time (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded set-time response: got" & Response'Image);
   end Test_Decode_Set_Time;

   procedure Test_Decode_Get_Listener (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Listener_Response := (
         Controller => 405419896,
         Address    => [192, 168, 1, 100],
         Port       => 60001,
         Interval   => 17);

      Reply : constant Packet := [
         16#17#, 16#92#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Listener_Response := Uhppoted.Lib.Decode.Get_Listener (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-listener response: got" & Response'Image);
   end Test_Decode_Get_Listener;

   procedure Test_Decode_Set_Listener (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Set_Listener_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#90#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Set_Listener_Response := Uhppoted.Lib.Decode.Set_Listener (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded set-listener response: got" & Response'Image);
   end Test_Decode_Set_Listener;

   procedure Test_Decode_Get_Status (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Status_Response := (
         Controller           => 405419896,
         System_Date          => (Year => 2022, Month => 8, Day => 23),
         System_Time          => (Hour => 9, Minute => 49, Second => 39),
         Door_1_Open          => False,
         Door_2_Open          => True,
         Door_3_Open          => False,
         Door_4_Open          => False,
         Door_1_Button        => False,
         Door_2_Button        => False,
         Door_3_Button        => False,
         Door_4_Button        => True,
         Relays               => 7,
         Inputs               => 9,
         System_Error         => 3,
         Special_Info         => 39,
         Event_Index          => 78,
         Event_Type           => 2,
         Event_Access_Granted => True,
         Event_Door           => 3,
         Event_Direction      => 1,
         Event_Card           => 8165537,
         Event_Timestamp      => (Year => 2022, Month => 8, Day => 23, Hour => 9, Minute => 47, Second => 6),
         Event_Reason         => 44,
         Sequence_No          => 0);

      Reply : constant Packet := [
         16#17#, 16#20#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#4e#, 16#00#, 16#00#, 16#00#, 16#02#, 16#01#, 16#03#, 16#01#,
         16#a1#, 16#98#, 16#7c#, 16#00#, 16#20#, 16#22#, 16#08#, 16#23#,  16#09#, 16#47#, 16#06#, 16#2c#, 16#00#, 16#01#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#01#, 16#03#, 16#09#, 16#49#, 16#39#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#27#, 16#07#, 16#09#, 16#22#, 16#08#, 16#23#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Status_Response := Uhppoted.Lib.Decode.Get_Status (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-status response: got" & Response'Image);
   end Test_Decode_Get_Status;

   procedure Test_Decode_Get_Listener_Address_Port (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Listener_Addr_Port_Response := (
         Controller => 405419896,
         Listener   => Network_Socket_Address (Addr => Inet_Addr ("192.168.1.100"), Port => Port_Type (60001)),
         Interval   => 17);

      Reply : constant Packet := [
         16#17#, 16#92#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Listener_Addr_Port_Response := Uhppoted.Lib.Decode.Get_Listener_Addr_Port (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-listener-addr:port response: got" & Response'Image);
   end Test_Decode_Get_Listener_Address_Port;

   procedure Test_Decode_Set_Listener_Address_Port (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Set_Listener_Addr_Port_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#90#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Set_Listener_Addr_Port_Response := Uhppoted.Lib.Decode.Set_Listener_Addr_Port (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded set-listener-addr:port response: got" & Response'Image);
   end Test_Decode_Set_Listener_Address_Port;

   procedure Test_Decode_Get_Door (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Door_Response := (
         Controller => 405419896,
         Door       => 3,
         Mode       => 2,
         OpenDelay  => 7);

      Reply : constant Packet := [
         16#17#, 16#82#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#02#, 16#07#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Door_Response := Uhppoted.Lib.Decode.Get_Door (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-door response: got" & Response'Image);
   end Test_Decode_Get_Door;

   procedure Test_Decode_Set_Door (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Set_Door_Response := (
         Controller => 405419896,
         Door       => 3,
         Mode       => 2,
         OpenDelay  => 7);

      Reply : constant Packet := [
         16#17#, 16#80#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#02#, 16#07#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Set_Door_Response := Uhppoted.Lib.Decode.Set_Door (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded set-door response: got" & Response'Image);
   end Test_Decode_Set_Door;

   procedure Test_Decode_Set_Door_Passcodes (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Set_Door_Passcodes_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#8c#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Set_Door_Passcodes_Response := Uhppoted.Lib.Decode.Set_Door_Passcodes (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded set-door-passcodes response: got" & Response'Image);
   end Test_Decode_Set_Door_Passcodes;

   procedure Test_Decode_Open_Door (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Open_Door_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#40#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Open_Door_Response := Uhppoted.Lib.Decode.Open_Door (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded open-door response: got" & Response'Image);
   end Test_Decode_Open_Door;

   procedure Test_Decode_Get_Cards (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Cards_Response := (
         Controller => 405419896,
         Cards      => 13579);

      Reply : constant Packet := [
         16#17#, 16#58#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#0b#, 16#35#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Cards_Response := Uhppoted.Lib.Decode.Get_Cards (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-cards response: got" & Response'Image);
   end Test_Decode_Get_Cards;

   procedure Test_Decode_Get_Card (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Card_Response := (
         Controller => 405419896,
         Card       => 10058400,
         Start_Date => (Year => 2024, Month => 1, Day => 1),
         End_Date   => (Year => 2024, Month => 12, Day => 31),
         Door_1     => 1,
         Door_2     => 0,
         Door_3     => 17,
         Door_4     => 1,
         PIN        => 999999);

      Reply : constant Packet := [
         16#17#, 16#5a#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#a0#, 16#7a#, 16#99#, 16#00#, 16#20#, 16#24#, 16#01#, 16#01#,
         16#20#, 16#24#, 16#12#, 16#31#, 16#01#, 16#00#, 16#11#, 16#01#,  16#3f#, 16#42#, 16#0f#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Card_Response := Uhppoted.Lib.Decode.Get_Card (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-card response: got" & Response'Image);
   end Test_Decode_Get_Card;

   procedure Test_Decode_Get_Card_Not_Found (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Card_Response := (
         Controller => 405419896,
         Card       => 0,
         Start_Date => (Year => 1, Month => 1, Day => 1),
         End_Date   => (Year => 1, Month => 1, Day => 1),
         Door_1     => 0,
         Door_2     => 0,
         Door_3     => 0,
         Door_4     => 0,
         PIN        => 0);

      Reply : constant Packet := [
         16#17#, 16#5a#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Card_Response := Uhppoted.Lib.Decode.Get_Card (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-card response: got" & Response'Image);
   end Test_Decode_Get_Card_Not_Found;

   procedure Test_Decode_Get_Card_At_Index (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Card_At_Index_Response := (
         Controller => 405419896,
         Card       => 10058400,
         Start_Date => (Year => 2024, Month => 1, Day => 1),
         End_Date   => (Year => 2024, Month => 12, Day => 31),
         Door_1     => 1,
         Door_2     => 0,
         Door_3     => 17,
         Door_4     => 1,
         PIN        => 999999);

      Reply : constant Packet := [
         16#17#, 16#5c#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#a0#, 16#7a#, 16#99#, 16#00#, 16#20#, 16#24#, 16#01#, 16#01#,
         16#20#, 16#24#, 16#12#, 16#31#, 16#01#, 16#00#, 16#11#, 16#01#,  16#3f#, 16#42#, 16#0f#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Card_At_Index_Response := Uhppoted.Lib.Decode.Get_Card_At_Index (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-card-at-index response: got" & Response'Image);
   end Test_Decode_Get_Card_At_Index;

   procedure Test_Decode_Put_Card (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Put_Card_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#50#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Put_Card_Response := Uhppoted.Lib.Decode.Put_Card (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded put-card response: got" & Response'Image);
   end Test_Decode_Put_Card;

   procedure Test_Decode_Delete_Card (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Delete_Card_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#52#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Delete_Card_Response := Uhppoted.Lib.Decode.Delete_Card (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded delete-card response: got" & Response'Image);
   end Test_Decode_Delete_Card;

   procedure Test_Decode_Delete_All_Cards (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Delete_All_Cards_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#54#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Delete_All_Cards_Response := Uhppoted.Lib.Decode.Delete_All_Cards (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded delete-all-cards response: got" & Response'Image);
   end Test_Decode_Delete_All_Cards;

   procedure Test_Decode_Get_Event_Index (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Get_Event_Index_Response := (
         Controller => 405419896,
         Index      => 13579);

      Reply : constant Packet := [
         16#17#, 16#b4#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#0b#, 16#35#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Get_Event_Index_Response := Uhppoted.Lib.Decode.Get_Event_Index (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded get-event-index response: got" & Response'Image);
   end Test_Decode_Get_Event_Index;

   procedure Test_Decode_Set_Event_Index (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Set_Event_Index_Response := (
         Controller => 405419896,
         Ok         => true);

      Reply : constant Packet := [
         16#17#, 16#b2#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Set_Event_Index_Response := Uhppoted.Lib.Decode.Set_Event_Index (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded set-event-index response: got" & Response'Image);
   end Test_Decode_Set_Event_Index;

   procedure Test_Decode_Record_Special_Events (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Record_Special_Events_Response := (
         Controller => 405419896,
         Ok         => True);

      Reply : constant Packet := [
         16#17#, 16#8e#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Response : constant Record_Special_Events_Response := Uhppoted.Lib.Decode.Record_Special_Events (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded record-special-events response: got" & Response'Image);
   end Test_Decode_Record_Special_Events;

end Uhppoted.Lib.Decode.Tests;
