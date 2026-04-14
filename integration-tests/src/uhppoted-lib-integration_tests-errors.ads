with AUnit.Test_Cases;

package Uhppoted.Lib.Integration_Tests.Errors is
   use AUnit.Test_Cases;

   type Integration_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Integration_Test);
   overriding function  Name           (T : Integration_Test) return AUnit.Message_String;
   overriding procedure Set_Up_Case    (T : in out Integration_Test);
   overriding procedure Tear_Down_Case (T : in out Integration_Test);
   overriding procedure Set_Up         (T : in out Integration_Test);
   overriding procedure Tear_Down      (T : in out Integration_Test);

private
   task Listen;

   procedure Test_Invalid_SOM    (T : in out Test_Case'Class);
   procedure Test_Invalid_OpCode (T : in out Test_Case'Class);
   procedure Test_Timeout        (T : in out Test_Case'Class);

end Uhppoted.Lib.Integration_Tests.Errors;
