with AUnit.Test_Cases;

package Uhppoted.Lib.Integration_Tests is
   type Integration_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding function Name (T : Integration_Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out Integration_Test);

   procedure Test_Find_Controllers (T : in out AUnit.Test_Cases.Test_Case'Class);

end Uhppoted.Lib.Integration_Tests;
