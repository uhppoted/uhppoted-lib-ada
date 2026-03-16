with AUnit.Test_Cases;

package Uhppoted.Lib.Decode.Invalid_SOM_Tests is
   type SOM_Test is new AUnit.Test_Cases.Test_Case with null record;
   type SOM_Test_Access is access all SOM_Test;

   overriding function  Name           (T : SOM_Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out SOM_Test);
{{ range $test := .Tests }}
   procedure {{ printf "Test_%v_Invalid_SOM" $test.Name | rpad 31 }} (T : in out AUnit.Test_Cases.Test_Case'Class);
{{- end }}

end Uhppoted.Lib.Decode.Invalid_SOM_Tests;
