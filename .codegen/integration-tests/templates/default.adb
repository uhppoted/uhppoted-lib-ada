with Ada.Exceptions;
with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;
with Uhppoted.Lib.Integration_Tests.Expected;

package body Uhppoted.Lib.Integration_Tests.Default is
   use AUnit.Assertions;

   Socket : Socket_Type;
   Port   : constant Port_Type := 60005;

   U : constant UHPPOTE := (
      Bind_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 0),

      Broadcast_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("255.255.255.255"),
         Port => Port),

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
{{- range $ix,$test := .Tests }}
      Register_Routine (T, {{ printf "Test_%v'Access," .Name | rpad 32 }} "{{ .Name }}");{{end}}
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
{{ range $ix,$test := .Tests }}{{if not (skip .Name)}}{{- template "unittest" $test }}{{end}}{{end}}
   --  custom test cases
   procedure Test_Get_Card_Not_Found (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      declare
         Unused : constant Card_Record := Get_Card (U, 405419896, 10058401, 0.5);
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

   begin
      declare
         Unused : constant Card_Record := Get_Card_At_Index (U, 405419896, 136, 0.5);
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

   begin
      declare
         Unused : constant Card_Record := Get_Card_At_Index (U, 405419896, 137, 0.5);
      begin
         Assert (False, "Expected 'card deleted' error");
      end;

   exception
      when Card_Deleted_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Card_Deleted_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Card_At_Index_Deleted;

end Uhppoted.Lib.Integration_Tests.Default;
{{ define "unittest" }}
   procedure Test_{{ printf "%v" .Name }} (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
{{ range $var := .Vars }}
      {{ $var }}
{{ end }}
      V : constant {{ .Returns.Type }} := {{ .Function }} (U{{ range $arg := .Args }}, {{ $arg }}{{ end }}, 0.5);
   begin
      Assert (V = Expected.{{ .Name }}, "invalid result" & V'Image);
   end Test_{{ printf "%v" .Name}};
{{ end }}