with Ada.Exceptions;
with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;
with Uhppoted.Lib.Integration_Tests.Expected;

package body Uhppoted.Lib.Integration_Tests.UDP is
   use AUnit.Assertions;

   Socket : Socket_Type;
   Port   : constant Port_Type := 60004;

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
      return AUnit.Format ("UDP tests");
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
      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      Uhppoted.Lib.Integration_Tests.Stub.ListenUDP (Socket => Socket, Port => Port);
   end Listen;

   procedure Test_Get_Controller (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

      V : constant Boolean := Set_IPv4 (U, C, Inet_Addr ("192.168.1.125"), Inet_Addr ("255.255.255.0"), Inet_Addr ("192.168.1.1"), 0.5);
   begin
      Assert (V = Expected.Set_IPv4, "invalid result" & V'Image);
   end Test_Set_IPv4;

   procedure Test_Get_Time (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

      V : constant Door_Type := Get_Door (U, C, 4, 0.5);
   begin
      Assert (V = Expected.Get_Door, "invalid result" & V'Image);
   end Test_Get_Door;

   procedure Test_Set_Door (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.UDP);

      V : constant Door_Type := Set_Door (U, C, 4, To_Control_Mode (2), 17, 0.5);
   begin
      Assert (V = Expected.Set_Door, "invalid result" & V'Image);
   end Test_Set_Door;

   procedure Test_Set_Door_Passcodes (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

      V : constant Card_Type := Get_Card (U, C, 10058400, 0.5);
   begin
      Assert (V = Expected.Get_Card, "invalid result" & V'Image);
   end Test_Get_Card;

   procedure Test_Get_Card_At_Index (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.UDP);

      V : constant Card_Type := Get_Card_At_Index (U, C, 135, 0.5);
   begin
      Assert (V = Expected.Get_Card_At_Index, "invalid result" & V'Image);
   end Test_Get_Card_At_Index;

   procedure Test_Put_Card (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.UDP);

      Card : constant Uhppoted.Lib.Card_Type := (
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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

      V : constant Event_Type := Get_Event (U, C, 13579, 0.5);
   begin
      Assert (V = Expected.Get_Event, "invalid result" & V'Image);
   end Test_Get_Event;

   procedure Test_Get_Event_Index (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

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
                                  Protocol => Uhppoted.Lib.UDP);

      V : constant Boolean := Record_Special_Events (U, C, True, 0.5);
   begin
      Assert (V = Expected.Record_Special_Events, "invalid result" & V'Image);
   end Test_Record_Special_Events;

   --  custom tests
   procedure Test_Get_Card_Not_Found (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => 405419896,
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.UDP);
   begin
      declare
         Unused : constant Card_Type := Get_Card (U, C, 10058401, 0.5);
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
                                  Protocol => Uhppoted.Lib.UDP);
   begin
      declare
         Unused : constant Card_Type := Get_Card_At_Index (U, C, 136, 0.5);
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
                                  Protocol => Uhppoted.Lib.UDP);
   begin
      declare
         Unused : constant Card_Type := Get_Card_At_Index (U, C, 137, 0.5);
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
                                               Port => 54321),
                                  Protocol => Uhppoted.Lib.UDP);
   begin
      declare
         Unused : constant Controller_Record := Get_Controller (U, C, 0.5);
      begin
         Assert (False, "Expected 'connection refused' error");
      end;

   exception
      when E : Socket_Error =>
         declare
            Err : constant Error_Type := Resolve_Exception (E);
         begin
            if Err /= Connection_Refused then
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
                                  Protocol => Uhppoted.Lib.UDP);
   begin
      declare
         Unused : constant Event_Type := Get_Event (U, C, 24680, 0.5);
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
                                  Protocol => Uhppoted.Lib.UDP);
   begin
      declare
         Unused : constant Event_Type := Get_Event (U, C, 98765, 0.5);
      begin
         Assert (False, "Expected 'event not found' error");
      end;

   exception
      when Event_Overwritten_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Event_Overwritten_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Event_Overwritten;

end Uhppoted.Lib.Integration_Tests.UDP;
