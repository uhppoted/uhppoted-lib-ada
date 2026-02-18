with Uhppoted.Lib.Integration_Tests;

package body Uhppoted.Lib.Integration_Tests_Suite is
   use AUnit.Test_Suites;

   Result : aliased AUnit.Test_Suites.Test_Suite;
   Integration_Test : aliased Uhppoted.Lib.Integration_Tests.Integration_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Integration_Test'Access);

      return Result'Access;
   end Suite;

end Uhppoted.Lib.Integration_Tests_Suite;
