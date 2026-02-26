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

end Uhppoted.Lib.Decode.Tests;
