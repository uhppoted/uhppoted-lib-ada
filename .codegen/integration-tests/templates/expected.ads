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

{{- define "controllers" }}[{{  range $ix,$v := .Returns.Value }}{{if $ix}},{{end}}
      ({{ range $jx,$vv := $v.Response }}{{if $jx}},{{end}}
        {{ field $vv.Name | rpad 8 }} => {{ value $vv.Type $vv.Value }}{{end}}){{ end }}
     ]{{- end }}

{{- define "controller" }}({{ range $ix,$v := .Returns.Value }}{{if $ix}},{{end}}
      {{ field $v.Name | rpad 8 }} => {{ value $v.Type $v.Value }}{{end}}){{ end }}

{{- define "listener" }}({{ with $v := .Returns.Value }}
     AddrPort => Network_Socket_Address (Addr => Inet_Addr ("{{ gets $v "address"}}"), Port => Port_Type ({{ get $v "port"}})),
     Interval => {{ get $v "interval" }}{{end}}){{ end }}

{{- define "status" }}({{ with $v := .Returns.Value }}
     System_Date_Time => {{ get $v "system.datetime"}},
     Doors => [1 => (Open     => {{ get $v "door-1.open"     }},
                     Button   => {{ get $v "door-1.button"   }},
                     Unlocked => {{ get $v "door-1.unlocked" }}),
               2 => (Open     => {{ get $v "door-2.open"     }},
                     Button   => {{ get $v "door-2.button"   }},
                     Unlocked => {{ get $v "door-2.unlocked" }}),
               3 => (Open     => {{ get $v "door-3.open"     }},
                     Button   => {{ get $v "door-3.button"   }},
                     Unlocked => {{ get $v "door-3.unlocked" }}),
               4 => (Open     => {{ get $v "door-4.open"     }},
                     Button   => {{ get $v "door-4.button"   }},
                     Unlocked => {{ get $v "door-4.unlocked" }})],
     Alarms => (Flags       => {{ get $v "inputs" }},
                Fire        => {{ get $v "alarms.fire" }},
                Lock_Forced => {{ get $v "alarms.lock-forced" }}),
     System_Error => {{ get $v "system.error" }},
     Special_Info => {{ get $v "system.special-info" }},
     Event => (Index          => {{ get $v "event.index"          }},
               Event          => {{ get $v "event.type"           }},
               Timestamp      => {{ get $v "event.timestamp"      }},
               Door           => {{ get $v "event.door"           }},
               Direction      => {{ get $v "event.direction"      }},
               Card           => {{ get $v "event.card"           }},
               Access_Granted => {{ get $v "event.access-granted" }},
               Reason         => {{ get $v "event.reason"         }}){{ end }}){{ end }}

{{- define "door" }}({{ with $v := .Returns.Value }}
     Mode      => {{ get $v "mode"}},
     OpenDelay => {{ get $v "delay"}}{{ end }}){{ end }}
