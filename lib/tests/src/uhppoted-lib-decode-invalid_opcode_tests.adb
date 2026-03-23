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

      Register_Routine (T, Test_Get_Controller_Invalid_OpCode'Access,  "test Get_Controller invalid op-code decode");
      --  Register_Routine (T, Test_Set_IPv4_Invalid_SOM'Access,        "test Set_IPv4 invalid SOM decode");
      --  Register_Routine (T, Test_Get_Time_Invalid_SOM'Access,        "test Get_Time invalid SOM decode");
      --  Register_Routine (T, Test_Set_Time_Invalid_SOM'Access,        "test Set_Time invalid SOM decode");
      --  Register_Routine (T, Test_Get_Listener_Invalid_SOM'Access,    "test Get_Listener invalid SOM decode");
      --  Register_Routine (T, Test_Set_Listener_Invalid_SOM'Access,    "test Set_Listener invalid SOM decode");
      --  Register_Routine (T, Test_Get_Status_Invalid_SOM'Access,      "test Get_Status invalid SOM decode");
      --  Register_Routine (T, Test_Get_Door_Invalid_SOM'Access,        "test Get_Door invalid SOM decode");
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

end Uhppoted.Lib.Decode.Invalid_OpCode_Tests;
