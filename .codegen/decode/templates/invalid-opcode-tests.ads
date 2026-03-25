with AUnit.Test_Cases;

package Uhppoted.Lib.Decode.Invalid_OpCode_Tests is
   type OpCode_Test is new AUnit.Test_Cases.Test_Case with null record;
   type OpCode_Test_Access is access all OpCode_Test;

   overriding function  Name           (T : OpCode_Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out OpCode_Test);
{{ range $test := .Tests }}
   procedure {{ printf "Test_%v_Invalid_OpCode" $test.Name | rpad 34 }} (T : in out AUnit.Test_Cases.Test_Case'Class);
{{- end }}

end Uhppoted.Lib.Decode.Invalid_OpCode_Tests;
