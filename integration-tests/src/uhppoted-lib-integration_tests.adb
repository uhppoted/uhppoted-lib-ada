with AUnit.Assertions;

package body Uhppoted.Lib.Integration_Tests is
   use AUnit.Assertions;

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("integrations tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Find_Controllers'Access, "Test Find_Controllers");
   end Register_Tests;

   procedure Test_Find_Controllers (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      C405419896 : constant Uhppoted.Lib.Controller := (
         ID       => 405419896,
         Address  => (192, 168, 1, 100),
         Netmask  => (255, 255, 255, 0),
         Gateway  => (192, 168, 1, 1),
         MAC      => (16#00#, 16#12#, 16#23#, 16#34#, 16#45#, 16#56#),
         Firmware => "0892",
         Date     => (
            Year  => 2018,
            Month => 11,
            Day   => 5));

      C303986753 : constant Uhppoted.Lib.Controller := (
         ID       => 303986753,
         Address  => (192, 168, 1, 100),
         Netmask  => (255, 255, 255, 0),
         Gateway  => (192, 168, 1, 1),
         MAC      => (16#52#, 16#fd#, 16#fc#, 16#07#, 16#21#, 16#82#),
         Firmware => "0892",
         Date     => (
            Year  => 2019,
            Month => 8,
            Day   => 15));

      C201020304 : constant Uhppoted.Lib.Controller := (
         ID       => 201020304,
         Address  => (192, 168, 1, 101),
         Netmask  => (255, 255, 255, 0),
         Gateway  => (192, 168, 1, 1),
         MAC      => (16#52#, 16#fd#, 16#fc#, 16#07#, 16#21#, 16#82#),
         Firmware => "0662",
         Date     => (
            Year  => 2020,
            Month => 1,
            Day   => 1));

      Controllers : constant Controller_List := Find_Controllers;
   begin
      Assert (Controllers'Length = 3, "expected 3 controllers, got" & Controllers'Length'Image);
      Assert (Controllers (1) = C405419896, "invalid 405419896 controller record");
      Assert (Controllers (2) = C303986753, "invalid 303986753 controller record");
      Assert (Controllers (3) = C201020304, "invalid 201020304 controller record");

   end Test_Find_Controllers;

end Uhppoted.Lib.Integration_Tests;
