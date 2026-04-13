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
   Invalid_SOM_Reply : constant Reply_List := [
      1 => [
         16#18#, 16#94#, 16#00#, 16#00#, 16#90#, 16#53#, 16#fb#, 16#0b#,  16#c0#, 16#a8#, 16#01#, 16#64#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
         16#c0#, 16#a8#, 16#01#, 16#01#, 16#00#, 16#12#, 16#23#, 16#34#,  16#45#, 16#56#, 16#08#, 16#92#, 16#20#, 16#18#, 16#11#, 16#05#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ]
   ];

   Invalid_OpCode_Reply : constant Reply_List := [
      1 => [
         16#17#, 16#94#, 16#00#, 16#00#, 16#90#, 16#53#, 16#fb#, 16#0b#,  16#20#, 16#25#, 16#11#, 16#01#, 16#12#, 16#34#, 16#56#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
      ]
   ];

end Uhppoted.Lib.Integration_Tests.Stub.Replies;
{{- define "reply"}}
   {{ .Name }}_Reply : constant Reply_List := [
      {{- range $reply := .Replies }}
      {{ $reply.Index }} => [
      {{- range $bytes := $reply.Reply }}
         {{ $bytes }}{{ end }}
      ]{{ $reply.Comma }}{{ end }}
   ];
{{ end }}
