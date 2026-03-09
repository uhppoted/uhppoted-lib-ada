with AUnit.Assertions;
with Ada.Strings.Unbounded;

package body Uhppoted.Lib.Decode.Tests is
   use AUnit.Assertions;
   use Ada.Strings.Unbounded;

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
      Register_Routine (T, Test_Decode_Get_Status'Access,   "test decode Get_Status response");
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

end Uhppoted.Lib.Decode.Tests;
