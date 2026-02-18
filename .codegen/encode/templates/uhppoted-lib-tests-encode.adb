with AUnit.Assertions;
with Uhppoted.Lib.Types;
with Uhppoted.Lib.Encode;

package body Uhppoted.Lib.Encode.Tests is
   use AUnit.Assertions;
   use Uhppoted.Lib.Types;

   overriding function Name (T : Encoder_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("encoder tests");
   end Name;

   overriding procedure Register_Tests (T : in out Encoder_Test) is
      use AUnit.Test_Cases.Registration;
   begin
{{- template "register" . }}
   end Register_Tests;
{{ range $test := .Tests }}
{{- template "unittest" $test }}
{{- end }}

end Uhppoted.Lib.Encode.Tests;

{{- define "register"}}
{{- range $test := .Tests }}
      Register_Routine (T, {{ printf "Test_Encode_%s'Access," $test.Name | rpad 36 }} "{{ $test.Description }}");
{{- end }}{{end}}

{{- define "unittest"}}
   procedure Test_Encode_{{ .Name }} (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant Packet := [
{{- range $bytes := .Expected }}
         {{ $bytes }}{{ end }}
      ];

      Request : constant Packet := Uhppoted.Lib.Encode.{{ var .Request }} ({{ args .Args }});
   begin
      Assert (Request = Expected, "incorrectly encoded {{ .Request }} request: got" & Request'Image);
   end Test_Encode_{{ .Name }};
{{ end }}
