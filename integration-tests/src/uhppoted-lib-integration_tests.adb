with Ada.Text_IO;
with Ada.Assertions;

package body Uhppoted.Lib.Integration_Tests is

   procedure Test_Find_Controllers  is
      C405419896 : Uhppoted.Lib.Controller := (
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

      C303986753 : Uhppoted.Lib.Controller := (
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

      C201020304 : Uhppoted.Lib.Controller := (
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
      if Controllers'Length /= 3 then
         raise Ada.Assertions.Assertion_Error with "expected 3 controllers, got" & Controllers'Length'Image;
      end if;

      if Controllers (1) /= C405419896 then
         raise Ada.Assertions.Assertion_Error with "invalid 405419896 controller record";
      end if;

      if Controllers (2) /= C303986753 then
         raise Ada.Assertions.Assertion_Error with "invalid 303986753 controller record";
      end if;

      if Controllers (3) /= C201020304 then
         raise Ada.Assertions.Assertion_Error with "invalid 201020304 controller record";
      end if;

   end Test_Find_Controllers;

end Uhppoted.Lib.Integration_Tests;
