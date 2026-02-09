with AUnit.Test_Suites;
with Uhppoted.Lib.Decode.Tests;

function Test_Suite return AUnit.Test_Suites.Access_Test_Suite is
   use AUnit.Test_Suites;
   Result : constant Access_Test_Suite := New_Suite;
begin
   Add_Test (Result, new Uhppoted.Lib.Decode.Tests.Decoder_Test);
   return Result;
end Test_Suite;
