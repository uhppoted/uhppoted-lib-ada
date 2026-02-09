with AUnit.Assertions;

package body Uhppoted.Lib.Decode.Tests is
   use AUnit.Assertions;

   overriding function Name (T : Decoder_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("decoder tests");
   end Name;

   overriding procedure Register_Tests (T : in out Decoder_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_BCD'Access, "Test BCD");
      Register_Routine (T, Test_Decode_Get_Controller'Access, "Test Get_Controller");
   end Register_Tests;

   procedure Test_BCD (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant String := "1234";
      Result   : constant String := BCD_To_String (BCD'(16#12#, 16#34#));
   begin
      Assert (Result = Expected, "BCD incorrectly decoded: got" & Result'Image);
   end Test_BCD;

   procedure Test_Decode_Get_Controller (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Uhppoted.Lib.Controller := (
         ID       => 405419896,
         Address  => [192, 168, 1, 100],
         Netmask  => [255, 255, 255, 0],
         Gateway  => [192, 168, 1, 1],
         MAC      => [16#00#, 16#12#, 16#23#, 16#34#, 16#45#, 16#56#],
         Firmware => "0892",
         Date     => (
            Year  => 2018,
            Month => 11,
            Day   => 5));

      Reply : constant Packet := [
         16#17#, 16#94#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
         16#c0#, 16#a8#, 16#01#, 16#01#, 16#00#, 16#12#, 16#23#, 16#34#,  16#45#, 16#56#, 16#08#, 16#92#, 16#20#, 16#18#, 16#11#, 16#05#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Result : constant Uhppoted.Lib.Controller := Uhppoted.Lib.Decode.Get_Controller (Reply);
   begin
      Assert (Result = Expected, "incorrectly decoded get-controller response: got" & Result'Image);
   end Test_Decode_Get_Controller;

end Uhppoted.Lib.Decode.Tests;
