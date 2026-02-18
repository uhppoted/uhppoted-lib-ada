with AUnit.Assertions;
with Uhppoted.Lib.Types;
with Uhppoted.Lib.Encode;

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
      Register_Routine (T, Test_Encode_Find_Controllers'Access, "Test encode Find_Controllers request");
      Register_Routine (T, Test_Encode_Get_Controller'Access,   "Test encode Get_Controller request");
   end Register_Tests;

   procedure Test_Encode_Find_Controllers (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
         16#17#, 16#94#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Controller (0);
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

      Request : constant Packet := Uhppoted.Lib.Encode.Get_Controller (405419896);
   begin
      Assert (Request = Expected, "incorrectly encoded get-controller request: got" & Request'Image);
   end Test_Encode_Get_Controller;


end Uhppoted.Lib.Encode.Tests;
