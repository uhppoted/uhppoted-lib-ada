with Ada.Strings.Unbounded;

package Uhppoted.Lib.Integration_Tests.Expected is
   use Ada.Strings.Unbounded;

{{-  range $ix,$test := .Tests }}
{{- if $ix -}}{{ end }}
{{- template "expected" $test }}
{{- end }}

end Uhppoted.Lib.Integration_Tests.Expected;
{{- define "expected"}}

   {{ .Name }} : constant {{ .Returns.Type }} := {{render .Returns.Template .}};
{{- end }}
{{- define "unknown"    }}null{{ end }}
{{- define "boolean"    }}{{ .Returns.Value }}{{ end }}
{{- define "datetime"   }}{{ .Returns.Value }}{{ end }}

{{- define "controller" }}({{ range $ix,$v := .Returns.Value.Response }}{{if $ix}},{{end}}
      {{ field $v.Name | rpad 8 }} => {{ value $v.Type $v.Value }}{{end}}){{ end }}

{{- define "controllers" }}[{{  range $ix,$v := .Returns.Value }}{{if $ix}},{{end}}
      ({{ range $jx,$vv := $v.Response }}{{if $jx}},{{end}}
        {{ field $vv.Name | rpad 8 }} => {{ value $vv.Type $vv.Value }}{{end}}){{ end }}
     ]{{- end }}

{{- define "status" }}({{ range $ix,$v := .Returns.Value }}{{if $ix}},{{end}}
        {{ field $v.Name | rpad 8 }} => {{ value $v.Type $v.Value }}{{end}}){{ end }}
