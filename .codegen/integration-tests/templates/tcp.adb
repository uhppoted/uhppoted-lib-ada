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
{{- range $ix,$test := .Tests }}
      Register_Routine (T, {{ printf "Test_%v'Access," .Name | rpad 40 }} "{{ .Name }}");{{end}}
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
{{ range $ix,$test := .Tests }}{{if not (skip .Name)}}{{- template "tcptest" $test }}{{end}}{{end}}
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
{{ define "tcptest" }}
   procedure Test_{{ printf "%v" .Name }} (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C : constant Controller := (ID       => {{ index .Args 0 }},
                                  DestAddr => (Family => Family_Inet,
                                               Addr => Inet_Addr ("127.0.0.1"),
                                               Port => Port),
                                  Protocol => Uhppoted.Lib.TCP);
{{ range $var := .Vars }}
      {{ $var }}
{{ end }}
      V : constant {{ .Returns.Type }} := {{ .Function }} (U, C{{ range $arg := (slice .Args  1) }}, {{ $arg }}{{ end }}, 0.5);
   begin
      Assert (V = Expected.{{ .Name }}, "invalid result" & V'Image);
   end Test_{{ printf "%v" .Name}};
{{ end }}