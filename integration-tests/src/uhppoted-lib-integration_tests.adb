with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;

package body Uhppoted.Lib.Integration_Tests is
   use AUnit.Assertions;
   use GNAT.Sockets;

   U : constant UHPPOTE := (
      Bind_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 0),

      Broadcast_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("255.255.255.255"),
         Port => 60005),

      Listen_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 60001),

      Debug => True);

   C : constant Controller := (
      Controller => 405419896,
      Address    => (Family => GNAT.Sockets.Family_Inet, Addr => Inet_Addr ("127.0.0.1"), Port => 60000),
      Protocol   => Default);

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("integration tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Find_Controllers'Access,         "Test Find_Controllers");
      Register_Routine (T, Test_Get_Controller_By_ID'Access,     "Test Get_Controller (by ID)");
      Register_Routine (T, Test_Get_Controller_By_Struct'Access, "Test Get_Controller (by struct)");
   end Register_Tests;

   --  overriding procedure Set_Up (T : in out Integration_Test) is
   --  begin
   --     null;
   --  end Set_Up;

   --  overriding procedure Tear_Down (T : in out Integration_Test) is
   --  begin
   --     null;
   --  end Tear_Down;

   task body Listen is
   begin
      Uhppoted.Lib.Integration_Tests.Stub.Listen;
   end Listen;

   procedure Test_Find_Controllers (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C405419896 : constant Controller_Record := (
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

      C303986753 : constant Controller_Record := (
         ID       => 303986753,
         Address  => [192, 168, 1, 100],
         Netmask  => [255, 255, 255, 0],
         Gateway  => [192, 168, 1, 1],
         MAC      => [16#52#, 16#fd#, 16#fc#, 16#07#, 16#21#, 16#82#],
         Firmware => "0892",
         Date     => (
            Year  => 2019,
            Month => 8,
            Day   => 15));

      C201020304 : constant Controller_Record := (
         ID       => 201020304,
         Address  => [192, 168, 1, 101],
         Netmask  => [255, 255, 255, 0],
         Gateway  => [192, 168, 1, 1],
         MAC      => [16#52#, 16#fd#, 16#fc#, 16#07#, 16#21#, 16#82#],
         Firmware => "0662",
         Date     => (
            Year  => 2020,
            Month => 1,
            Day   => 1));

      Controllers : constant Controller_Record_List := Find_Controllers (U);
   begin
      Assert (Controllers'Length = 3, "expected 3 controllers, got" & Controllers'Length'Image);
      Assert (Controllers (1) = C405419896, "invalid 405419896 controller record");
      Assert (Controllers (2) = C303986753, "invalid 303986753 controller record");
      Assert (Controllers (3) = C201020304, "invalid 201020304 controller record");

   end Test_Find_Controllers;

   procedure Test_Get_Controller_By_ID (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C405419896 : constant Controller_Record := (
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

      V : constant Controller_Record := Get_Controller (U, C.Controller);
   begin
      Assert (V = C405419896, "invalid 405419896 controller record");
   end Test_Get_Controller_By_ID;

   procedure Test_Get_Controller_By_Struct (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C405419896 : constant Controller_Record := (
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

      V : constant Controller_Record := Get_Controller (U, C);
   begin
      Assert (V = C405419896, "invalid 405419896 controller record");
   end Test_Get_Controller_By_Struct;

end Uhppoted.Lib.Integration_Tests;
