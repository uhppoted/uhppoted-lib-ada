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

      Register_Routine (T, Test_Get_Controller_Invalid_SOM'Access,  "test Get_Controller invalid SOM decode");
      Register_Routine (T, Test_Set_IPv4_Invalid_SOM'Access,        "test Set_IPv4 invalid SOM decode");
      Register_Routine (T, Test_Get_Time_Invalid_SOM'Access,        "test Get_Time invalid SOM decode");
      Register_Routine (T, Test_Set_Time_Invalid_SOM'Access,        "test Set_Time invalid SOM decode");
      Register_Routine (T, Test_Get_Listener_Invalid_SOM'Access,    "test Get_Listener invalid SOM decode");
      Register_Routine (T, Test_Set_Listener_Invalid_SOM'Access,    "test Set_Listener invalid SOM decode");
      Register_Routine (T, Test_Get_Status_Invalid_SOM'Access,      "test Get_Status invalid SOM decode");
      Register_Routine (T, Test_Get_Door_Invalid_SOM'Access,        "test Get_Door invalid SOM decode");
   end Register_Tests;

   procedure Test_Get_Controller_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#94#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
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
   end Test_Get_Controller_Invalid_SOM;

   procedure Test_Set_IPv4_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#96#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
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
   end Test_Set_IPv4_Invalid_SOM;

   procedure Test_Get_Time_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#32#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
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
   end Test_Get_Time_Invalid_SOM;

   procedure Test_Set_Time_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#30#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
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
   end Test_Set_Time_Invalid_SOM;

   procedure Test_Get_Listener_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#92#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#61#, 16#ea#, 16#11#, 16#00#,
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
   end Test_Get_Listener_Invalid_SOM;

   procedure Test_Set_Listener_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#90#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#01#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
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
   end Test_Set_Listener_Invalid_SOM;

   procedure Test_Get_Status_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#20#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#4e#, 16#00#, 16#00#, 16#00#, 16#02#, 16#01#, 16#03#, 16#01#,
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
   end Test_Get_Status_Invalid_SOM;

   procedure Test_Get_Door_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [
         16#13#, 16#82#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#03#, 16#02#, 16#07#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
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
   end Test_Get_Door_Invalid_SOM;

end Uhppoted.Lib.Decode.Invalid_SOM_Tests;
