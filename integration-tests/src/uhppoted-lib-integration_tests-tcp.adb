with Ada.Exceptions;
with AUnit.Assertions;
with Ada.Strings.Fixed;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;
with Uhppoted.Lib.Integration_Tests.Expected;

package body Uhppoted.Lib.Integration_Tests.TCP is
   use AUnit.Assertions;

   Socket : Socket_Type;
   Port   : constant Port_Type := 60003;

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

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("TCP tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Get_Controller'Access,              "Get_Controller");
      Register_Routine (T, Test_Set_IPv4'Access,                    "Set_IPv4");
      Register_Routine (T, Test_Get_Time'Access,                    "Get_Time");
      Register_Routine (T, Test_Set_Time'Access,                    "Set_Time");
      Register_Routine (T, Test_Get_Listener'Access,                "Get_Listener");
      Register_Routine (T, Test_Set_Listener'Access,                "Set_Listener");
      Register_Routine (T, Test_Get_Status'Access,                  "Get_Status");
      Register_Routine (T, Test_Get_Status_No_Event'Access,         "Get_Status_No_Event");
      Register_Routine (T, Test_Get_Door'Access,                    "Get_Door");
      Register_Routine (T, Test_Set_Door'Access,                    "Set_Door");
      Register_Routine (T, Test_Set_Door_Passcodes'Access,          "Set_Door_Passcodes");
      Register_Routine (T, Test_Open_Door'Access,                   "Open_Door");
      Register_Routine (T, Test_Get_Cards'Access,                   "Get_Cards");
      Register_Routine (T, Test_Get_Card'Access,                    "Get_Card");
      Register_Routine (T, Test_Get_Card_Not_Found'Access,          "Get_Card_Not_Found");
      Register_Routine (T, Test_Get_Card_At_Index'Access,           "Get_Card_At_Index");
      Register_Routine (T, Test_Get_Card_At_Index_Not_Found'Access, "Get_Card_At_Index_Not_Found");
      Register_Routine (T, Test_Get_Card_At_Index_Deleted'Access,   "Get_Card_At_Index_Deleted");
      Register_Routine (T, Test_Put_Card'Access,                    "Put_Card");
      Register_Routine (T, Test_Delete_Card'Access,                 "Delete_Card");
      Register_Routine (T, Test_Delete_All_Cards'Access,            "Delete_All_Cards");
      Register_Routine (T, Test_Get_Event'Access,                   "Get_Event");
      Register_Routine (T, Test_Get_Event_Not_Found'Access,         "Get_Event_Not_Found");
      Register_Routine (T, Test_Get_Event_Overwritten'Access,       "Get_Event_Overwritten");
      Register_Routine (T, Test_Get_Event_Index'Access,             "Get_Event_Index");
      Register_Routine (T, Test_Set_Event_Index'Access,             "Set_Event_Index");
      Register_Routine (T, Test_Record_Special_Events'Access,       "Record_Special_Events");
      Register_Routine (T, Test_Get_Time_Profile'Access,            "Get_Time_Profile");
      Register_Routine (T, Test_Set_Time_Profile'Access,            "Set_Time_Profile");
      Register_Routine (T, Test_Clear_Time_Profiles'Access,         "Clear_Time_Profiles");
      Register_Routine (T, Test_Add_Task'Access,                    "Add_Task");
      Register_Routine (T, Test_Refresh_Task_List'Access,           "Refresh_Task_List");
      Register_Routine (T, Test_Clear_Task_List'Access,             "Clear_Task_List");
      Register_Routine (T, Test_Set_Pc_Control'Access,              "Set_Pc_Control");
      Register_Routine (T, Test_Set_Interlock'Access,               "Set_Interlock");
      Register_Routine (T, Test_Activate_Keypads'Access,            "Activate_Keypads");
      Register_Routine (T, Test_Get_Antipassback'Access,            "Get_Antipassback");
      Register_Routine (T, Test_Set_Antipassback'Access,            "Set_Antipassback");
      Register_Routine (T, Test_Restore_Default_Parameters'Access,  "Restore_Default_Parameters");
      Register_Routine (T, Test_Connection_Refused'Access,            "connection refused");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Integration_Test) is
   begin
      null;
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Integration_Test) is
   begin
      Close_Socket (Socket);
   end Tear_Down_Case;

   overriding procedure Set_Up (T : in out Integration_Test) is
   begin
      null;
   end Set_Up;

   overriding procedure Tear_Down (T : in out Integration_Test) is
   begin
      null;
   end Tear_Down;

   task body Listen is
   begin
      Create_Socket (Socket);
      Uhppoted.Lib.Integration_Tests.Stub.ListenTCP (Socket => Socket, Port => Port);
   end Listen;

   procedure Test_Get_Controller (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Controller_Record := Get_Controller (U, C, 0.5);
   begin
      Assert (V = Expected.Get_Controller, "invalid result" & V'Image);
   end Test_Get_Controller;

   procedure Test_Set_IPv4 (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      Address : constant Inet_Addr_Type := Inet_Addr ("192.168.1.125");
      Netmask : constant Inet_Addr_Type := Inet_Addr ("255.255.255.0");
      Gateway : constant Inet_Addr_Type := Inet_Addr ("192.168.1.1");

      V : constant Boolean := Set_IPv4 (U, C, Address, Netmask, Gateway, 0.5);
   begin
      Assert (V = Expected.Set_IPv4, "invalid result" & V'Image);
   end Test_Set_IPv4;

   procedure Test_Get_Time (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant DateTime := Get_Time (U, C, 0.5);
   begin
      Assert (V = Expected.Get_Time, "invalid result" & V'Image);
   end Test_Get_Time;

   procedure Test_Set_Time (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant DateTime := Set_Time (U, C, (Year => 2025, Month => 11, Day => 4, Hour => 12, Minute => 34, Second => 56), 0.5);
   begin
      Assert (V = Expected.Set_Time, "invalid result" & V'Image);
   end Test_Set_Time;

   procedure Test_Get_Listener (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Listener_Record := Get_Listener (U, C, 0.5);
   begin
      Assert (V = Expected.Get_Listener, "invalid result" & V'Image);
   end Test_Get_Listener;

   procedure Test_Set_Listener (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419897,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Set_Listener (U, C, (Family_Inet, Inet_Addr ("192.168.1.100"), 60001), 17, 0.5);
   begin
      Assert (V = Expected.Set_Listener, "invalid result" & V'Image);
   end Test_Set_Listener;

   procedure Test_Get_Status (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Controller_Status := Get_Status (U, C, 0.5);
   begin
      Assert (V = Expected.Get_Status, "invalid result" & V'Image);
   end Test_Get_Status;

   procedure Test_Get_Status_No_Event (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419897,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Controller_Status := Get_Status (U, C, 0.5);
   begin
      Assert (V = Expected.Get_Status_No_Event, "invalid result" & V'Image);
   end Test_Get_Status_No_Event;

   procedure Test_Get_Door (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Door_Record := Get_Door (U, C, 4, 0.5);
   begin
      Assert (V = Expected.Get_Door, "invalid result" & V'Image);
   end Test_Get_Door;

   procedure Test_Set_Door (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Door_Record := Set_Door (U, C, 4, To_Control_Mode (2), 17, 0.5);
   begin
      Assert (V = Expected.Set_Door, "invalid result" & V'Image);
   end Test_Set_Door;

   procedure Test_Set_Door_Passcodes (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      Passcodes : constant Uhppoted.Lib.Passcodes_List (1 .. 4) := [1 => 12345, 2 => 54321, 3 => 999999, 4 => 0];

      V : constant Boolean := Set_Door_Passcodes (U, C, 4, Passcodes, 0.5);
   begin
      Assert (V = Expected.Set_Door_Passcodes, "invalid result" & V'Image);
   end Test_Set_Door_Passcodes;

   procedure Test_Open_Door (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Open_Door (U, C, 4, 0.5);
   begin
      Assert (V = Expected.Open_Door, "invalid result" & V'Image);
   end Test_Open_Door;

   procedure Test_Get_Cards (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Unsigned_32 := Get_Cards (U, C, 0.5);
   begin
      Assert (V = Expected.Get_Cards, "invalid result" & V'Image);
   end Test_Get_Cards;

   procedure Test_Get_Card (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Card_Record := Get_Card (U, C, 10058400, 0.5);
   begin
      Assert (V = Expected.Get_Card, "invalid result" & V'Image);
   end Test_Get_Card;

   procedure Test_Get_Card_At_Index (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Card_Record := Get_Card_At_Index (U, C, 135, 0.5);
   begin
      Assert (V = Expected.Get_Card_At_Index, "invalid result" & V'Image);
   end Test_Get_Card_At_Index;

   procedure Test_Put_Card (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      Card : constant Uhppoted.Lib.Card_Record := (
         Card       => 10058400,
         Start_Date => (Year => 2025, Month => 1, Day => 1),
         End_Date   => (Year => 2025, Month => 12, Day => 31),
         Door_1     => 1,
         Door_2     => 0,
         Door_3     => 17,
         Door_4     => 1,
         PIN        => 999999);

      V : constant Boolean := Put_Card (U, C, Card, 0.5);
   begin
      Assert (V = Expected.Put_Card, "invalid result" & V'Image);
   end Test_Put_Card;

   procedure Test_Delete_Card (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Delete_Card (U, C, 10058400, 0.5);
   begin
      Assert (V = Expected.Delete_Card, "invalid result" & V'Image);
   end Test_Delete_Card;

   procedure Test_Delete_All_Cards (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Delete_All_Cards (U, C, 0.5);
   begin
      Assert (V = Expected.Delete_All_Cards, "invalid result" & V'Image);
   end Test_Delete_All_Cards;

   procedure Test_Get_Event (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Controller_Event := Get_Event (U, C, 13579, 0.5);
   begin
      Assert (V = Expected.Get_Event, "invalid result" & V'Image);
   end Test_Get_Event;

   procedure Test_Get_Event_Index (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Unsigned_32 := Get_Event_Index (U, C, 0.5);
   begin
      Assert (V = Expected.Get_Event_Index, "invalid result" & V'Image);
   end Test_Get_Event_Index;

   procedure Test_Set_Event_Index (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Set_Event_Index (U, C, 13579, 0.5);
   begin
      Assert (V = Expected.Set_Event_Index, "invalid result" & V'Image);
   end Test_Set_Event_Index;

   procedure Test_Record_Special_Events (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Record_Special_Events (U, C, True, 0.5);
   begin
      Assert (V = Expected.Record_Special_Events, "invalid result" & V'Image);
   end Test_Record_Special_Events;

   procedure Test_Get_Time_Profile (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Time_Profile := Get_Time_Profile (U, C, 37, 0.5);
   begin
      Assert (V = Expected.Get_Time_Profile, "invalid result" & V'Image);
   end Test_Get_Time_Profile;

   procedure Test_Set_Time_Profile (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      Profile : constant Uhppoted.Lib.Time_Profile := (
         Start_Date => (Year => 2025, Month => 11, Day => 26),
         End_Date   => (Year => 2025, Month => 12, Day => 29),
         Weekdays   => (Monday    => True,
                        Tuesday   => True,
                        Wednesday => False,
                        Thursday  => True,
                        Friday    => False,
                        Saturday  => True,
                        Sunday    => True),
        Segments   => [1 => (Start_Time => (Hour => 8, Minute => 30), End_Time => (Hour => 9, Minute => 45)),
                       2 => (Start_Time => (Hour => 11, Minute => 35), End_Time => (Hour => 13, Minute => 15)),
                       3 => (Start_Time => (Hour => 14, Minute => 1), End_Time => (Hour => 17, Minute => 59))],
         Linked_Profile => 19);

      V : constant Boolean := Set_Time_Profile (U, C, 37, Profile, 0.5);
   begin
      Assert (V = Expected.Set_Time_Profile, "invalid result" & V'Image);
   end Test_Set_Time_Profile;

   procedure Test_Clear_Time_Profiles (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Clear_Time_Profiles (U, C, 0.5);
   begin
      Assert (V = Expected.Clear_Time_Profiles, "invalid result" & V'Image);
   end Test_Clear_Time_Profiles;

   procedure Test_Add_Task (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      TaskT : constant Uhppoted.Lib.Task_Record := (
         Task_ID    => To_Task_Type (2),
         Start_Date => (Year => 2025, Month => 1, Day => 1),
         End_Date   => (Year => 2025, Month => 12, Day => 31),
         Weekdays   => (Monday    => True,
                        Tuesday   => True,
                        Wednesday => False,
                        Thursday  => True,
                        Friday    => False,
                        Saturday  => True,
                        Sunday    => True),
         Start_Time => (Hour => 8, Minute => 45),
         Door       => 3,
         More_Cards => 7);

      V : constant Boolean := Add_Task (U, C, TaskT, 0.5);
   begin
      Assert (V = Expected.Add_Task, "invalid result" & V'Image);
   end Test_Add_Task;

   procedure Test_Refresh_Task_List (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Refresh_Task_List (U, C, 0.5);
   begin
      Assert (V = Expected.Refresh_Task_List, "invalid result" & V'Image);
   end Test_Refresh_Task_List;

   procedure Test_Clear_Task_List (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Clear_Task_List (U, C, 0.5);
   begin
      Assert (V = Expected.Clear_Task_List, "invalid result" & V'Image);
   end Test_Clear_Task_List;

   procedure Test_Set_Pc_Control (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Set_PC_Control (U, C, True, 0.5);
   begin
      Assert (V = Expected.Set_Pc_Control, "invalid result" & V'Image);
   end Test_Set_Pc_Control;

   procedure Test_Set_Interlock (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Set_Interlock (U, C, To_Interlock (8), 0.5);
   begin
      Assert (V = Expected.Set_Interlock, "invalid result" & V'Image);
   end Test_Set_Interlock;

   procedure Test_Activate_Keypads (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      Keypads : constant Uhppoted.Lib.Keypads := [1 => True, 2 => True, 3 => False, 4 => True];

      V : constant Boolean := Activate_Keypads (U, C, Keypads, 0.5);
   begin
      Assert (V = Expected.Activate_Keypads, "invalid result" & V'Image);
   end Test_Activate_Keypads;

   procedure Test_Get_Antipassback (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Antipassback := Get_Antipassback (U, C, 0.5);
   begin
      Assert (V = Expected.Get_Antipassback, "invalid result" & V'Image);
   end Test_Get_Antipassback;

   procedure Test_Set_Antipassback (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Set_Antipassback (U, C, To_Antipassback (2), 0.5);
   begin
      Assert (V = Expected.Set_Antipassback, "invalid result" & V'Image);
   end Test_Set_Antipassback;

   procedure Test_Restore_Default_Parameters (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);

      V : constant Boolean := Restore_Default_Parameters (U, C, 0.5);
   begin
      Assert (V = Expected.Restore_Default_Parameters, "invalid result" & V'Image);
   end Test_Restore_Default_Parameters;

   --  custom tests
   procedure Test_Get_Card_Not_Found (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);
   begin
      declare
         Unused : constant Card_Record := Get_Card (U, C, 10058401, 0.5);
      begin
         Assert (False, "Expected 'card not found' error");
      end;

   exception
      when Card_Not_Found_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Card_Not_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Card_Not_Found;

   procedure Test_Get_Card_At_Index_Not_Found (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);
   begin
      declare
         Unused : constant Card_Record := Get_Card_At_Index (U, C, 136, 0.5);
      begin
         Assert (False, "Expected 'card not found' error");
      end;

   exception
      when Card_Not_Found_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Card_Not_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Card_At_Index_Not_Found;

   procedure Test_Get_Card_At_Index_Deleted (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);
   begin
      declare
         Unused : constant Card_Record := Get_Card_At_Index (U, C, 137, 0.5);
      begin
         Assert (False, "Expected 'card deleted' error");
      end;

   exception
      when Card_Deleted_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Card_Deleted_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Card_At_Index_Deleted;

   procedure Test_Connection_Refused (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => 12345),
                                  Protocol => Uhppoted.Lib.TCP);
   begin
      declare
         Unused : constant Controller_Record := Get_Controller (U, C, 0.5);
      begin
         Assert (False, "Expected 'connection refused' error");
      end;

   exception
      --  NTS: Resolve_Exception returns Cannot_Resolve_Error for TCP
      when E : Socket_Error =>
         declare
            Err : constant Error_Type := Resolve_Exception (E);
            Msg : constant String := Ada.Exceptions.Exception_Message (E);
         begin
            if Err /= Connection_Refused
              and then (Err /= Cannot_Resolve_Error
                        or else Ada.Strings.Fixed.Index (Msg, "CONNECTION_REFUSED") = 0)
            then
               Assert (False, "Expected 'connection refused', got: " & Err'Image);
            end if;
         end;
      when E : others =>
         Assert (False, "Expected Socket_Error.Connection_Refused, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Connection_Refused;

   procedure Test_Get_Event_Not_Found (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);
   begin
      declare
         Unused : constant Controller_Event := Get_Event (U, C, 24680, 0.5);
      begin
         Assert (False, "Expected 'event not found' error");
      end;

   exception
      when Event_Not_Found_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Event_Not_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Event_Not_Found;

   procedure Test_Get_Event_Overwritten (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);
   begin
      declare
         Unused : constant Controller_Event := Get_Event (U, C, 98765, 0.5);
      begin
         Assert (False, "Expected 'event not found' error");
      end;

   exception
      when Event_Overwritten_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Event_Overwritten_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Event_Overwritten;

end Uhppoted.Lib.Integration_Tests.TCP;
