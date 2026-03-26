with AUnit.Assertions;

package body Uhppoted.Lib.Decode.Invalid_OpCode_Tests is
   use AUnit.Assertions;

   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   overriding function Name (T : OpCode_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("invalid op-code tests");
   end Name;

   overriding procedure Register_Tests (T : in out OpCode_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Get_Controller_Invalid_OpCode'Access,  "test decode Get_Controller with invalid opcode");
      Register_Routine (T, Test_Set_IPv4_Invalid_OpCode'Access,        "test decode Set_IPv4 with invalid opcode");
      Register_Routine (T, Test_Get_Time_Invalid_OpCode'Access,        "test decode Get_Time with invalid opcode");
      Register_Routine (T, Test_Set_Time_Invalid_OpCode'Access,        "test decode Set_Time with invalid opcode");
      Register_Routine (T, Test_Get_Listener_Invalid_OpCode'Access,    "test decode Get_Listener with invalid opcode");
      Register_Routine (T, Test_Set_Listener_Invalid_OpCode'Access,    "test decode Set_Listener with invalid opcode");
      Register_Routine (T, Test_Get_Status_Invalid_OpCode'Access,      "test decode Get_Status with invalid opcode");
      Register_Routine (T, Test_Get_Listener_Address_Port_Invalid_OpCode'Access, "test decode Get_Listener_Address_Port with invalid opcode");
      Register_Routine (T, Test_Set_Listener_Address_Port_Invalid_OpCode'Access, "test decode Set_Listener_Address_Port with invalid opcode");
      Register_Routine (T, Test_Get_Door_Invalid_OpCode'Access,        "test decode Get_Door with invalid opcode");
      Register_Routine (T, Test_Set_Door_Invalid_OpCode'Access,        "test decode Set_Door with invalid opcode");
      Register_Routine (T, Test_Set_Door_Passcodes_Invalid_OpCode'Access, "test decode Set_Door_Passcodes with invalid opcode");
   end Register_Tests;

   procedure Test_Get_Controller_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#95#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
         16#c0#, 16#a8#, 16#01#, 16#01#, 16#00#, 16#12#, 16#23#, 16#34#,  16#45#, 16#56#, 16#08#, 16#92#, 16#20#, 16#18#, 16#11#, 16#05#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Get_Controller_Response := Uhppoted.Lib.Decode.Get_Controller (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Get_Controller_Invalid_OpCode;

   procedure Test_Set_IPv4_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#97#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Set_IPv4_Response := Uhppoted.Lib.Decode.Set_IPv4 (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Set_IPv4_Invalid_OpCode;

   procedure Test_Get_Time_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#33#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Get_Time_Response := Uhppoted.Lib.Decode.Get_Time (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Get_Time_Invalid_OpCode;

   procedure Test_Set_Time_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#31#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Set_Time_Response := Uhppoted.Lib.Decode.Set_Time (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Set_Time_Invalid_OpCode;

   procedure Test_Get_Listener_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#93#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Get_Listener_Response := Uhppoted.Lib.Decode.Get_Listener (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Get_Listener_Invalid_OpCode;

   procedure Test_Set_Listener_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#91#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Set_Listener_Response := Uhppoted.Lib.Decode.Set_Listener (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Set_Listener_Invalid_OpCode;

   procedure Test_Get_Status_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#21#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#4e#, 16#00#, 16#00#, 16#00#, 16#02#, 16#01#, 16#03#, 16#01#,
         16#a1#, 16#98#, 16#7c#, 16#00#, 16#20#, 16#22#, 16#08#, 16#23#,  16#09#, 16#47#, 16#06#, 16#2c#, 16#00#, 16#01#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#01#, 16#03#, 16#09#, 16#49#, 16#39#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#27#, 16#07#, 16#09#, 16#22#, 16#08#, 16#23#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Get_Status_Response := Uhppoted.Lib.Decode.Get_Status (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Get_Status_Invalid_OpCode;

   procedure Test_Get_Listener_Address_Port_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#93#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Get_Listener_Addr_Port_Response := Uhppoted.Lib.Decode.Get_Listener_Addr_Port (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Get_Listener_Address_Port_Invalid_OpCode;

   procedure Test_Set_Listener_Address_Port_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#91#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Set_Listener_Addr_Port_Response := Uhppoted.Lib.Decode.Set_Listener_Addr_Port (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Set_Listener_Address_Port_Invalid_OpCode;

   procedure Test_Get_Door_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#83#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#02#, 16#07#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Get_Door_Response := Uhppoted.Lib.Decode.Get_Door (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Get_Door_Invalid_OpCode;

   procedure Test_Set_Door_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#81#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#02#, 16#07#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Set_Door_Response := Uhppoted.Lib.Decode.Set_Door (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Set_Door_Invalid_OpCode;

   procedure Test_Set_Door_Passcodes_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#17#, 16#8d#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ];

      procedure Exec is
         Unused : constant Set_Door_Passcodes_Response := Uhppoted.Lib.Decode.Set_Door_Passcodes (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_Set_Door_Passcodes_Invalid_OpCode;

end Uhppoted.Lib.Decode.Invalid_OpCode_Tests;
