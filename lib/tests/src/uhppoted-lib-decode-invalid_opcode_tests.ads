with AUnit.Test_Cases;

package Uhppoted.Lib.Decode.Invalid_OpCode_Tests is
   type OpCode_Test is new AUnit.Test_Cases.Test_Case with null record;
   type OpCode_Test_Access is access all OpCode_Test;

   overriding function  Name           (T : OpCode_Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out OpCode_Test);

   procedure Test_Get_Controller_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_IPv4_Invalid_OpCode       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Time_Invalid_OpCode       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Time_Invalid_OpCode       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Listener_Invalid_OpCode   (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Listener_Invalid_OpCode   (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Status_Invalid_OpCode     (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Listener_Address_Port_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Listener_Address_Port_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Door_Invalid_OpCode       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Door_Invalid_OpCode       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Door_Passcodes_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Open_Door_Invalid_OpCode      (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Cards_Invalid_OpCode      (T : in out AUnit.Test_Cases.Test_Case'Class);

end Uhppoted.Lib.Decode.Invalid_OpCode_Tests;
