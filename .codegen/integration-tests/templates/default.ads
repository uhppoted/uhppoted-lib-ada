with AUnit.Test_Cases;

package Uhppoted.Lib.Integration_Tests.Default is
   use AUnit.Test_Cases;

   type Integration_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Integration_Test);
   overriding function  Name           (T : Integration_Test) return AUnit.Message_String;

private
   task Listen;
{{ range $ix,$test := .Tests }}
   procedure Test_{{ printf "%v" .Name | rpad 19 }} (T : in out Test_Case'Class);{{end}}

end Uhppoted.Lib.Integration_Tests.Default;
