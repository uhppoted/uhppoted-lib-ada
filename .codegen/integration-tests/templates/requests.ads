package Uhppoted.Lib.Integration_Tests.Stub.Requests is

   type Request is array (1 .. 64) of Interfaces.Unsigned_8;
{{ range $test := .Tests }}
{{- template "request" $test }}
{{- end }}
end Uhppoted.Lib.Integration_Tests.Stub.Requests;
{{- define "request"}}
   {{ .Name }}_Request : constant Request := [
      {{- range $bytes := .Request }}
      {{ $bytes }}{{ end }}
   ];
{{ end }}
