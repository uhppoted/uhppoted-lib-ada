with AUnit.Reporter.Text;
with AUnit.Run;
with Uhppoted.Lib.Test_Suite;

procedure Uhppoted_Lib_Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Uhppoted.Lib.Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Uhppoted_Lib_Tests;