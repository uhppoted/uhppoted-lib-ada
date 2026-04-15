with AUnit.Test_Cases;

package Uhppoted.Lib.Integration_Tests.UDP is
   use AUnit.Test_Cases;

   type Integration_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Integration_Test);
   overriding function  Name           (T : Integration_Test) return AUnit.Message_String;
   overriding procedure Set_Up_Case    (T : in out Integration_Test);
   overriding procedure Tear_Down_Case (T : in out Integration_Test);
   overriding procedure Set_Up         (T : in out Integration_Test);
   overriding procedure Tear_Down      (T : in out Integration_Test);

private
   Passcodes : Uhppoted.Lib.Passcodes_List (1 .. 4);

   task Listen;
{{ range $ix,$test := .Tests }}
   procedure Test_{{ printf "%v" .Name | rpad 27 }} (T : in out Test_Case'Class);{{end}}
   procedure Test_Connection_Refused (T : in out Test_Case'Class);

end Uhppoted.Lib.Integration_Tests.UDP;
