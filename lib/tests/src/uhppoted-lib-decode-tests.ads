with AUnit.Test_Cases;

package Uhppoted.Lib.Decode.Tests is
   type Decoder_Test is new AUnit.Test_Cases.Test_Case with null record;
   type Decode_Test_Access is access all Decoder_Test;

   overriding function Name (T : Decoder_Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out Decoder_Test);
   procedure Test_Decode_Get_Controller (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Decode_Set_IPv4 (T : in out AUnit.Test_Cases.Test_Case'Class);

end Uhppoted.Lib.Decode.Tests;
