with AUnit.Reporter.Text;
with AUnit.Run;
with Uhppoted.Lib.Integration_Tests_Suite;

procedure Uhppoted_Lib_Integration_Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Uhppoted.Lib.Integration_Tests_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Uhppoted_Lib_Integration_Tests;
