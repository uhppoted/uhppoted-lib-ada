with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;

package body Uhppoted.Lib.Integration_Tests.UDP is
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
         Port => 60014),

      Listen_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 60001),

      Debug => True);

   C : constant Controller := (
      Controller => 405419896,
      DestAddr   => (Family => GNAT.Sockets.Family_Inet, Addr => Inet_Addr ("127.0.0.1"), Port => 60004),
      others     => <>);

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("integration tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Get_Controller'Access, "Test Get_Controller");
   end Register_Tests;

   task body Listen is
   begin
      Uhppoted.Lib.Integration_Tests.Stub.Listen (Port => 60004);
   end Listen;

   procedure Test_Get_Controller (T : in out Test_Case'Class) is
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
   end Test_Get_Controller;

end Uhppoted.Lib.Integration_Tests.UDP;
