with Uhppoted.Lib.Integration_Tests.Default;
with Uhppoted.Lib.Integration_Tests.UDP;
with Uhppoted.Lib.Integration_Tests.TCP;

package body Uhppoted.Lib.Integration_Tests_Suite is
   use AUnit.Test_Suites;

   Result        : aliased AUnit.Test_Suites.Test_Suite;
   Default_Tests : aliased Uhppoted.Lib.Integration_Tests.Default.Integration_Test;
   UDP_Tests     : aliased Uhppoted.Lib.Integration_Tests.UDP.Integration_Test;
   TCP_Tests     : aliased Uhppoted.Lib.Integration_Tests.TCP.Integration_Test;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Default_Tests'Access);
      Add_Test (Result'Access, UDP_Tests'Access);
      Add_Test (Result'Access, TCP_Tests'Access);

      return Result'Access;
   end Suite;

end Uhppoted.Lib.Integration_Tests_Suite;
