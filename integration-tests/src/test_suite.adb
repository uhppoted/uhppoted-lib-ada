with AUnit.Test_Suites;
with Uhppoted.Lib.Integration_Tests;

function Test_Suite return AUnit.Test_Suites.Access_Test_Suite is
   use AUnit.Test_Suites;
   Tests : constant Access_Test_Suite := New_Suite;
begin
   Add_Test (Tests, new Uhppoted.Lib.Integration_Tests.Integration_Test);

   return Tests;
end Test_Suite;
