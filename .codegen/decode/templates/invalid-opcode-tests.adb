with AUnit.Assertions;

package body Uhppoted.Lib.Decode.Invalid_OpCode_Tests is
   use AUnit.Assertions;

   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   overriding function Name (T : OpCode_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("invalid op-code tests");
   end Name;

   overriding procedure Register_Tests (T : in out OpCode_Test) is
      use AUnit.Test_Cases.Registration;
   begin
{{- range $test := .Tests }}
      Register_Routine (T, {{ printf "Test_%v_Invalid_OpCode'Access," .Name | rpad 43 }} "test decode {{ .Name }} with invalid opcode");
{{- end }}
   end Register_Tests;
{{ range $test := .Tests }}
   procedure Test_{{ .Name }}_Invalid_OpCode (T : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Reply : constant Packet := [{{- range $bytes := .InvalidOpCode }}
         {{ $bytes }}{{ end }}
      ];

      procedure Exec is
         Unused : constant {{ var .Response }}_Response := Uhppoted.Lib.Decode.{{ var .Response }} (Reply);
      begin
         null;
      end Exec;
   begin
      Assert_Exception (Exec'Unrestricted_Access, "Expected 'invalid response' error");
   end Test_{{ .Name }}_Invalid_OpCode;
{{ end }}
end Uhppoted.Lib.Decode.Invalid_OpCode_Tests;
