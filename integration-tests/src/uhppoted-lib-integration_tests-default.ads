with AUnit.Test_Cases;

package Uhppoted.Lib.Integration_Tests.Default is
   use AUnit.Test_Cases;

   type Integration_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Integration_Test);
   overriding function Name (T : Integration_Test) return AUnit.Message_String;

   procedure Test_Find_Controllers (T : in out Test_Case'Class);
   procedure Test_Get_Controller_By_ID (T : in out Test_Case'Class);
   procedure Test_Get_Controller_By_Struct (T : in out Test_Case'Class);

   task Listen;

end Uhppoted.Lib.Integration_Tests.Default;
