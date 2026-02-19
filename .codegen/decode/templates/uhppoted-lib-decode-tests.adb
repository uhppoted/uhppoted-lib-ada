with AUnit.Assertions;

package body Uhppoted.Lib.Decode.Tests is
   use AUnit.Assertions;

   overriding function Name (T : Decoder_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("decoder tests");
   end Name;

   overriding procedure Register_Tests (T : in out Decoder_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      {{- template "register" . }}
      Register_Routine (T, Test_BCD'Access, "Test BCD");
   end Register_Tests;
{{ range $test := .Tests }}
{{- template "unittest" $test }}
{{- end }}
   procedure Test_BCD (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant String := "1234";
      Result   : constant String := BCD_To_String (BCD'(16#12#, 16#34#));
   begin
      Assert (Result = Expected, "BCD incorrectly decoded: got" & Result'Image);
   end Test_BCD;

end Uhppoted.Lib.Decode.Tests;

{{- define "register"}}
{{- range $test := .Tests }}
      Register_Routine (T, {{ printf "Test_Decode_%s'Access," $test.Name | rpad 32 }} "{{ $test.Description }}");
{{- end }}{{end}}

{{- define "unittest"}}
   procedure Test_Decode_{{ .Name }} (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Expected : constant {{ var .Response }}_Response := (
         ID       => 405419896,
         Address  => [192, 168, 1, 100],
         Netmask  => [255, 255, 255, 0],
         Gateway  => [192, 168, 1, 1],
         MAC      => [16#00#, 16#12#, 16#23#, 16#34#, 16#45#, 16#56#],
         Firmware => "0892",
         Date     => (
            Year  => 2018,
            Month => 11,
            Day   => 5));

      Reply : constant Packet := [
         {{- range $bytes := .Reply }}
         {{ $bytes }}{{ end }}
      ];

      Response : constant {{ var .Response }}_Response := Uhppoted.Lib.Decode.{{ var .Response }} (Reply);
   begin
      Assert (Response = Expected, "incorrectly decoded {{ .Response }} response: got" & Response'Image);
   end Test_Decode_{{ .Name }};
{{ end }}
