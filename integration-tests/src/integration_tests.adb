with AUnit.Reporter.Text;
with AUnit.Run;
with Test_Suite;

procedure Integration_Tests is
   procedure Runner is new AUnit.Run.Test_Runner (Test_Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end Integration_Tests;
