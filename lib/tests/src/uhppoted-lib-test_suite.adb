with Uhppoted.Lib.Decode.Tests;
with Uhppoted.Lib.Encode.Tests;

package body Uhppoted.Lib.Test_Suite is
   use AUnit.Test_Suites;

   Result : aliased AUnit.Test_Suites.Test_Suite;
   DecoderTest : aliased Uhppoted.Lib.Decode.Tests.Decoder_Test;
   EncoderTest : aliased Uhppoted.Lib.Encode.Tests.Encoder_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, EncoderTest'Access);
      Add_Test (Result'Access, DecoderTest'Access);

      return Result'Access;
   end Suite;
end Uhppoted.Lib.Test_Suite;