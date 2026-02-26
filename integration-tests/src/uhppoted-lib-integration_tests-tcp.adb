with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;
with Uhppoted.Lib.Integration_Tests.Expected;

package body Uhppoted.Lib.Integration_Tests.TCP is
   use AUnit.Assertions;

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
      DestAddr   => (Family => GNAT.Sockets.Family_Inet, Addr => Inet_Addr ("127.0.0.1"), Port => 60003),
      Protocol   => Uhppoted.Lib.TCP);

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("TCP tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Get_Controller'Access, "Test Get_Controller");
      Register_Routine (T, Test_Set_IPv4'Access,       "Set_IPv4");
   end Register_Tests;

   task body Listen is
   begin
      Uhppoted.Lib.Integration_Tests.Stub.ListenTCP (Port => 60003);
   end Listen;

   procedure Test_Get_Controller (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Controller_Record := Get_Controller (U, C);
   begin
      Assert (V = Expected.Get_Controller, "invalid controller record");
   end Test_Get_Controller;

   procedure Test_Set_IPv4 (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Addr    : constant Inet_Addr_Type := Inet_Addr ("192.168.1.125");
      Netmask : constant Inet_Addr_Type := Inet_Addr ("255.255.255.0");
      Gateway : constant Inet_Addr_Type := Inet_Addr ("192.168.1.1");
      V       : constant Boolean        := Set_IPv4 (U, C, Addr, Netmask, Gateway);
   begin
      Assert (V = Expected.Set_IPv4, "invalid Set_IPv4 response");
   end Test_Set_IPv4;

end Uhppoted.Lib.Integration_Tests.TCP;
