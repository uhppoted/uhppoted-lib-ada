with Ada.Containers.Vectors;

package Uhppoted.Lib.Integration_Tests.Stub.Events is

   type Message is array (1 .. 64) of Interfaces.Unsigned_8;
{{ range $test := .Tests }}
{{- template "listen-event" $test }}
{{- end }}
   type Event is record
      Msg : Message;
   end record;

   package Events_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Event);

   subtype Events_List is Events_Vector.Vector;

   Events : constant Events_List := [
{{- range $ix,$test := .Tests }}{{if $ix}},{{end}}
      (Msg => {{ .Name }}_Event){{- end }}
   ];

end Uhppoted.Lib.Integration_Tests.Stub.Events;
{{- define "listen-event"}}
   {{ .Name }}_Event : constant Message := [
      {{- range $bytes := .Message }}
      {{ $bytes }}{{ end }}
   ];
{{ end }}
