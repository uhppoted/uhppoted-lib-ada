with Ada.Exceptions;
with AUnit.Assertions;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub;

package body Uhppoted.Lib.Integration_Tests.Errors is
   use AUnit.Assertions;

   Socket : Socket_Type;
   Port   : constant Port_Type := 59999;

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
      return AUnit.Format ("error tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Invalid_SOM'Access,    "invalid SOM");
      Register_Routine (T, Test_Invalid_OpCode'Access, "invalid op-code");
      Register_Routine (T, Test_Timeout'Access,        "timeout");
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

   procedure Test_Invalid_SOM (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      declare
         Unused : constant Controller_Record := Get_Controller (U, 201020304, 0.5);
      begin
         Assert (False, "Expected an 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Invalid_SOM;

   procedure Test_Invalid_OpCode (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      declare
         Unused : constant DateTime := Get_Time (U, 201020304, 0.5);
      begin
         Assert (False, "Expected an 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Invalid_OpCode;

   procedure Test_Timeout (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

   begin
      declare
         Unused : constant Controller_Record := Get_Controller (U, 303986753, 0.5);
      begin
         Assert (False, "Expected a timeout error");
      end;

   exception
      when Timeout_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Timeout_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Timeout;

end Uhppoted.Lib.Integration_Tests.Errors;
