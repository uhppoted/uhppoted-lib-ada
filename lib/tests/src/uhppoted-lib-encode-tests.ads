with AUnit.Test_Cases;

package Uhppoted.Lib.Encode.Tests is
   type Encoder_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Encoder_Test);
   overriding function Name (T : Encoder_Test) return AUnit.Message_String;

   procedure Test_Encode_Find_Controllers (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Get_Controller (T : in out AUnit.Test_Cases.Test_Case'Class);

end Uhppoted.Lib.Encode.Tests;
