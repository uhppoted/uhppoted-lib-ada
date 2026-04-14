with Ada.Exceptions;
with AUnit.Assertions;

package body Uhppoted.Lib.Decode.Invalid_SOM_Tests is
   use AUnit.Assertions;

   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   overriding function Name (T : SOM_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("invalid SOM tests");
   end Name;

   overriding procedure Register_Tests (T : in out SOM_Test) is
      use AUnit.Test_Cases.Registration;
   begin

      Register_Routine (T, Test_Get_Controller_Invalid_SOM'Access,  "test decode Get_Controller with invalid SOM");
      Register_Routine (T, Test_Set_IPv4_Invalid_SOM'Access,        "test decode Set_IPv4 with invalid SOM");
      Register_Routine (T, Test_Get_Time_Invalid_SOM'Access,        "test decode Get_Time with invalid SOM");
      Register_Routine (T, Test_Set_Time_Invalid_SOM'Access,        "test decode Set_Time with invalid SOM");
      Register_Routine (T, Test_Get_Listener_Invalid_SOM'Access,    "test decode Get_Listener with invalid SOM");
      Register_Routine (T, Test_Set_Listener_Invalid_SOM'Access,    "test decode Set_Listener with invalid SOM");
      Register_Routine (T, Test_Get_Status_Invalid_SOM'Access,      "test decode Get_Status with invalid SOM");
      Register_Routine (T, Test_Get_Listener_Address_Port_Invalid_SOM'Access, "test decode Get_Listener_Address_Port with invalid SOM");
      Register_Routine (T, Test_Set_Listener_Address_Port_Invalid_SOM'Access, "test decode Set_Listener_Address_Port with invalid SOM");
      Register_Routine (T, Test_Get_Door_Invalid_SOM'Access,        "test decode Get_Door with invalid SOM");
      Register_Routine (T, Test_Set_Door_Invalid_SOM'Access,        "test decode Set_Door with invalid SOM");
      Register_Routine (T, Test_Set_Door_Passcodes_Invalid_SOM'Access, "test decode Set_Door_Passcodes with invalid SOM");
      Register_Routine (T, Test_Open_Door_Invalid_SOM'Access,       "test decode Open_Door with invalid SOM");
      Register_Routine (T, Test_Get_Cards_Invalid_SOM'Access,       "test decode Get_Cards with invalid SOM");
      Register_Routine (T, Test_Get_Card_Invalid_SOM'Access,        "test decode Get_Card with invalid SOM");
      Register_Routine (T, Test_Get_Card_Not_Found_Invalid_SOM'Access, "test decode Get_Card_Not_Found with invalid SOM");
      Register_Routine (T, Test_Get_Card_At_Index_Invalid_SOM'Access, "test decode Get_Card_At_Index with invalid SOM");
      Register_Routine (T, Test_Put_Card_Invalid_SOM'Access,        "test decode Put_Card with invalid SOM");
      Register_Routine (T, Test_Delete_Card_Invalid_SOM'Access,     "test decode Delete_Card with invalid SOM");
   end Register_Tests;

   procedure Test_Get_Controller_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#94#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
         16#c0#, 16#a8#, 16#01#, 16#01#, 16#00#, 16#12#, 16#23#, 16#34#,  16#45#, 16#56#, 16#08#, 16#92#, 16#20#, 16#18#, 16#11#, 16#05#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Controller_Response := Uhppoted.Lib.Decode.Get_Controller (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Controller_Invalid_SOM;

   procedure Test_Set_IPv4_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#96#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Set_IPv4_Response := Uhppoted.Lib.Decode.Set_IPv4 (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Set_IPv4_Invalid_SOM;

   procedure Test_Get_Time_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#32#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Time_Response := Uhppoted.Lib.Decode.Get_Time (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Time_Invalid_SOM;

   procedure Test_Set_Time_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#30#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Set_Time_Response := Uhppoted.Lib.Decode.Set_Time (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Set_Time_Invalid_SOM;

   procedure Test_Get_Listener_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#92#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Listener_Response := Uhppoted.Lib.Decode.Get_Listener (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Listener_Invalid_SOM;

   procedure Test_Set_Listener_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#90#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Set_Listener_Response := Uhppoted.Lib.Decode.Set_Listener (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Set_Listener_Invalid_SOM;

   procedure Test_Get_Status_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#20#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#4e#, 16#00#, 16#00#, 16#00#, 16#02#, 16#01#, 16#03#, 16#01#,
         16#a1#, 16#98#, 16#7c#, 16#00#, 16#20#, 16#22#, 16#08#, 16#23#,  16#09#, 16#47#, 16#06#, 16#2c#, 16#00#, 16#01#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#01#, 16#03#, 16#09#, 16#49#, 16#39#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#27#, 16#07#, 16#09#, 16#22#, 16#08#, 16#23#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Status_Response := Uhppoted.Lib.Decode.Get_Status (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Status_Invalid_SOM;

   procedure Test_Get_Listener_Address_Port_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#92#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Listener_Addr_Port_Response := Uhppoted.Lib.Decode.Get_Listener_Addr_Port (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Listener_Address_Port_Invalid_SOM;

   procedure Test_Set_Listener_Address_Port_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#90#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Set_Listener_Addr_Port_Response := Uhppoted.Lib.Decode.Set_Listener_Addr_Port (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Set_Listener_Address_Port_Invalid_SOM;

   procedure Test_Get_Door_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#82#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#02#, 16#07#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Door_Response := Uhppoted.Lib.Decode.Get_Door (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Door_Invalid_SOM;

   procedure Test_Set_Door_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#80#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#02#, 16#07#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Set_Door_Response := Uhppoted.Lib.Decode.Set_Door (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Set_Door_Invalid_SOM;

   procedure Test_Set_Door_Passcodes_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#8c#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Set_Door_Passcodes_Response := Uhppoted.Lib.Decode.Set_Door_Passcodes (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Set_Door_Passcodes_Invalid_SOM;

   procedure Test_Open_Door_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#40#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Open_Door_Response := Uhppoted.Lib.Decode.Open_Door (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Open_Door_Invalid_SOM;

   procedure Test_Get_Cards_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#58#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#0b#, 16#35#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Cards_Response := Uhppoted.Lib.Decode.Get_Cards (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Cards_Invalid_SOM;

   procedure Test_Get_Card_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#5a#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#a0#, 16#7a#, 16#99#, 16#00#, 16#20#, 16#24#, 16#01#, 16#01#,
         16#20#, 16#24#, 16#12#, 16#31#, 16#01#, 16#00#, 16#11#, 16#01#,  16#3f#, 16#42#, 16#0f#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Card_Response := Uhppoted.Lib.Decode.Get_Card (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Card_Invalid_SOM;

   procedure Test_Get_Card_Not_Found_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#5a#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Card_Response := Uhppoted.Lib.Decode.Get_Card (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Card_Not_Found_Invalid_SOM;

   procedure Test_Get_Card_At_Index_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#5c#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#a0#, 16#7a#, 16#99#, 16#00#, 16#20#, 16#24#, 16#01#, 16#01#,
         16#20#, 16#24#, 16#12#, 16#31#, 16#01#, 16#00#, 16#11#, 16#01#,  16#3f#, 16#42#, 16#0f#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Get_Card_At_Index_Response := Uhppoted.Lib.Decode.Get_Card_At_Index (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Get_Card_At_Index_Invalid_SOM;

   procedure Test_Put_Card_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#50#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Put_Card_Response := Uhppoted.Lib.Decode.Put_Card (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Put_Card_Invalid_SOM;

   procedure Test_Delete_Card_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#52#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

   begin
      declare
         Unused : constant Delete_Card_Response := Uhppoted.Lib.Decode.Delete_Card (Reply);
      begin
         Assert (False, "Expected 'invalid response' error");
      end;

   exception
      when Invalid_Response_Error =>
         null;
      when E : others =>
         Assert (False, "Expected Invalid_Response_Found_Error, got " & Ada.Exceptions.Exception_Name (E));
   end Test_Delete_Card_Invalid_SOM;

end Uhppoted.Lib.Decode.Invalid_SOM_Tests;
