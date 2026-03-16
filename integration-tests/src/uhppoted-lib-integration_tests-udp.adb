with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;
with Uhppoted.Lib.Integration_Tests.Expected;

package body Uhppoted.Lib.Integration_Tests.UDP is
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

      Debug => False);

   C : constant Controller := (ID       => 405419896,
                               DestAddr => (Family => Family_Inet,
                                            Addr => Inet_Addr ("127.0.0.1"),
                                            Port => 60004),
                               Protocol => Uhppoted.Lib.UDP);

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("UDP tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Get_Controller'Access,      "Test Get_Controller");
      Register_Routine (T, Test_Set_IPv4'Access,            "Set_IPv4");
      Register_Routine (T, Test_Get_Time'Access,            "Test Get_Time");
      Register_Routine (T, Test_Set_Time'Access,            "Set_Time");
      Register_Routine (T, Test_Get_Listener'Access,        "Get_Listener");
      Register_Routine (T, Test_Get_Status'Access,          "Get_Status");
      Register_Routine (T, Test_Get_Status_No_Event'Access, "Get_Status (no event)");
   end Register_Tests;

   task body Listen is
   begin
      Uhppoted.Lib.Integration_Tests.Stub.ListenUDP (Port => 60004);
   end Listen;

   procedure Test_Get_Controller (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Controller_Record := Get_Controller (U, C);
   begin
      Assert (V = Expected.Get_Controller, "invalid controller record" & V'Image);
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

   procedure Test_Get_Time (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant DateTime := Get_Time (U, C);
   begin
      Assert (V = Expected.Get_Time, "invalid controller date/time" & V'Image);
   end Test_Get_Time;

   procedure Test_Set_Time (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      DT : constant DateTime := (2025, 11, 4, 12, 34, 56);
      V  : constant DateTime := Set_Time (U, C, DT);
   begin
      Assert (V = Expected.Set_Time, "invalid controller date/time" & V'Image);
   end Test_Set_Time;

   procedure Test_Get_Listener (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Listener_Record := Get_Listener (U, C);
   begin
      Assert (V = Expected.Get_Listener, "invalid controller listener" & V'Image);
   end Test_Get_Listener;

   procedure Test_Get_Status (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Controller_Status := Get_Status (U, C);
   begin
      Assert (V = Expected.Get_Status, "invalid controller status" & V'Image);
   end Test_Get_Status;

   procedure Test_Get_Status_No_Event (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419897,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => 60004),
                                  Protocol => Uhppoted.Lib.UDP);

      V : constant Controller_Status := Get_Status (U, C);
   begin
      Assert (V = Expected.Get_Status_No_Event, "invalid controller status" & V'Image);
   end Test_Get_Status_No_Event;

end Uhppoted.Lib.Integration_Tests.UDP;
