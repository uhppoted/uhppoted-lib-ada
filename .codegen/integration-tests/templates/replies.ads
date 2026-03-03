with Ada.Containers.Vectors;

package Uhppoted.Lib.Integration_Tests.Stub.Replies is

   type Reply is array (1 .. 64) of Interfaces.Unsigned_8;

   package Reply_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Reply);

   subtype Reply_List is Reply_Vectors.Vector;
{{ range $test := .Tests }}
{{- template "reply" $test }}
{{- end }}
end Uhppoted.Lib.Integration_Tests.Stub.Replies;
{{- define "reply"}}
   {{ .Name }} : constant Reply_List := [
      {{- range $reply := .Replies }}
      {{ $reply.Index }} => [
      {{- range $bytes := $reply.Reply }}
         {{ $bytes }}{{ end }}
      ]{{ $reply.Comma }}{{ end }}
   ];
{{ end }}
