with AUnit.Test_Cases;

package Uhppoted.Lib.Tests.Encode is
   type Encoder_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Encoder_Test);
   overriding function Name (T : Encoder_Test) return AUnit.Message_String;
{{ range $test := .Tests }}
   procedure Test_Encode_{{ $test.Name }} (T : in out AUnit.Test_Cases.Test_Case'Class);
{{- end }}

end Uhppoted.Lib.Tests.Encode;
