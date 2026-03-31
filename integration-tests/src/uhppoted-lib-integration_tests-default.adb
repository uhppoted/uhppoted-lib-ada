with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;
with Uhppoted.Lib.Integration_Tests.Expected;

package body Uhppoted.Lib.Integration_Tests.Default is
   use AUnit.Assertions;

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

      Debug => False);

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("default tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Find_Controllers'Access,    "Find_Controllers");
      Register_Routine (T, Test_Get_Controller'Access,      "Get_Controller");
      Register_Routine (T, Test_Set_IPv4'Access,            "Set_IPv4");
      Register_Routine (T, Test_Get_Time'Access,            "Get_Time");
      Register_Routine (T, Test_Set_Time'Access,            "Set_Time");
      Register_Routine (T, Test_Get_Listener'Access,        "Get_Listener");
      Register_Routine (T, Test_Set_Listener'Access,        "Set_Listener");
      Register_Routine (T, Test_Get_Status'Access,          "Get_Status");
      Register_Routine (T, Test_Get_Status_No_Event'Access, "Get_Status_No_Event");
      Register_Routine (T, Test_Get_Door'Access,            "Get_Door");
      Register_Routine (T, Test_Set_Door'Access,            "Set_Door");
      Register_Routine (T, Test_Set_Door_Passcodes'Access,  "Set_Door_Passcodes");
      Register_Routine (T, Test_Open_Door'Access,           "Open_Door");
   end Register_Tests;

   task body Listen is
   begin
      Uhppoted.Lib.Integration_Tests.Stub.ListenUDP (Port => 60005);
   end Listen;

   procedure Test_Find_Controllers (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Controller_Record_List := Find_Controllers (U);
   begin
      Assert (V = Expected.Find_Controllers, "invalid result" & V'Image);
   end Test_Find_Controllers;

   procedure Test_Get_Controller (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Controller_Record := Get_Controller (U, 405419896);
   begin
      Assert (V = Expected.Get_Controller, "invalid result" & V'Image);
   end Test_Get_Controller;

   procedure Test_Set_IPv4 (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Boolean := Set_IPv4 (U, 405419896, Inet_Addr ("192.168.1.125"), Inet_Addr ("255.255.255.0"), Inet_Addr ("192.168.1.1"));
   begin
      Assert (V = Expected.Set_IPv4, "invalid result" & V'Image);
   end Test_Set_IPv4;

   procedure Test_Get_Time (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant DateTime := Get_Time (U, 405419896);
   begin
      Assert (V = Expected.Get_Time, "invalid result" & V'Image);
   end Test_Get_Time;

   procedure Test_Set_Time (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant DateTime := Set_Time (U, 405419896, (Year => 2025, Month => 11, Day => 4, Hour => 12, Minute => 34, Second => 56));
   begin
      Assert (V = Expected.Set_Time, "invalid result" & V'Image);
   end Test_Set_Time;

   procedure Test_Get_Listener (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Listener_Record := Get_Listener (U, 405419896);
   begin
      Assert (V = Expected.Get_Listener, "invalid result" & V'Image);
   end Test_Get_Listener;

   procedure Test_Set_Listener (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Boolean := Set_Listener (U, 405419897, (Family_Inet, Inet_Addr ("192.168.1.100"), 60001), 17);
   begin
      Assert (V = Expected.Set_Listener, "invalid result" & V'Image);
   end Test_Set_Listener;

   procedure Test_Get_Status (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Controller_Status := Get_Status (U, 405419896);
   begin
      Assert (V = Expected.Get_Status, "invalid result" & V'Image);
   end Test_Get_Status;

   procedure Test_Get_Status_No_Event (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Controller_Status := Get_Status (U, 405419897);
   begin
      Assert (V = Expected.Get_Status_No_Event, "invalid result" & V'Image);
   end Test_Get_Status_No_Event;

   procedure Test_Get_Door (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Door_Record := Get_Door (U, 405419896, 4);
   begin
      Assert (V = Expected.Get_Door, "invalid result" & V'Image);
   end Test_Get_Door;

   procedure Test_Set_Door (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Door_Record := Set_Door (U, 405419896, 4, To_Control_Mode (2), 17);
   begin
      Assert (V = Expected.Set_Door, "invalid result" & V'Image);
   end Test_Set_Door;

   procedure Test_Set_Door_Passcodes (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      Passcodes : constant Uhppoted.Lib.Passcodes_List (1 .. 4) := (1 => 12345, 2 => 54321, 3 => 999999, 4 => 0);

      V : constant Boolean := Set_Door_Passcodes (U, 405419896, 4, Passcodes);
   begin
      Assert (V = Expected.Set_Door_Passcodes, "invalid result" & V'Image);
   end Test_Set_Door_Passcodes;

   procedure Test_Open_Door (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      V : constant Boolean := Open_Door (U, 405419896, 4);
   begin
      Assert (V = Expected.Open_Door, "invalid result" & V'Image);
   end Test_Open_Door;

end Uhppoted.Lib.Integration_Tests.Default;
