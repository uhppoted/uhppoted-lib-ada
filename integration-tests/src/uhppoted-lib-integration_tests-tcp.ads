with AUnit.Test_Cases;

package Uhppoted.Lib.Integration_Tests.TCP is
   use AUnit.Test_Cases;

   type Integration_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Integration_Test);
   overriding function Name (T : Integration_Test) return AUnit.Message_String;

private
   task Listen;

   procedure Test_Get_Controller      (T : in out Test_Case'Class);
   procedure Test_Set_IPv4            (T : in out Test_Case'Class);
   procedure Test_Get_Time            (T : in out Test_Case'Class);
   procedure Test_Set_Time            (T : in out Test_Case'Class);
   procedure Test_Get_Listener        (T : in out Test_Case'Class);
   procedure Test_Set_Listener        (T : in out Test_Case'Class);
   procedure Test_Get_Status          (T : in out Test_Case'Class);
   procedure Test_Get_Status_No_Event (T : in out Test_Case'Class);

end Uhppoted.Lib.Integration_Tests.TCP;
