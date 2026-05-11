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
{{- define "unknown"  }}null{{ end }}
{{- define "boolean"  }}{{ .Returns.Value }}{{ end }}
{{- define "uint32"   }}{{ .Returns.Value }}{{ end }}
{{- define "datetime" }}{{ .Returns.Value }}{{ end }}

{{- define "controllers" }}[{{  range $ix,$v := .Returns.Value }}{{if $ix}},{{end}}
      ({{ range $jx,$vv := $v.Response }}{{if $jx}},{{end}}
        {{ field $vv.Name | rpad 8 }} => {{ value $vv.Type $vv.Value }}{{end}}){{ end }}
     ]{{- end }}

{{- define "controller" }}({{ range $ix,$v := .Returns.Value }}{{if $ix}},{{end}}
      {{ field $v.Name | rpad 8 }} => {{ value $v.Type $v.Value }}{{end}}){{ end }}

{{- define "listener" }}({{ with $v := .Returns.Value }}
     Listener => Network_Socket_Address (Addr => Inet_Addr ("{{ gets $v "address"}}"), Port => Port_Type ({{ get $v "port"}})),
     Interval => {{ get $v "interval" }}{{end}}){{ end }}

{{- define "status" }}({{ with $v := .Returns.Value }}
     State =>
        (System_Date_Time => {{ get $v "system.datetime"}},
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
         Special_Info => {{ get $v "system.special-info" }}),
     Event => (Index          => {{ get $v "event.index" }},
               Event          => To_Event_Type ({{ get $v "event.type"}}),
               Timestamp      => {{ get $v "event.timestamp" }},
               Door           => {{ get $v "event.door" }},
               Direction      => To_Event_Direction ({{ get $v "event.direction"}}),
               Card           => {{ get $v "event.card" }},
               Access_Granted => {{ get $v "event.access-granted" }},
               Reason         => To_Event_Reason ({{ get $v "event.reason"}})){{ end }}){{ end }}

{{- define "door" }}({{ with $v := .Returns.Value }}
     Mode      => To_Control_Mode ({{ get $v "mode"}}),
     OpenDelay => {{ get $v "delay"}}{{ end }}){{ end }}

{{- define "card" }}({{ with $v := .Returns.Value }}
     Card       => {{ get $v "card"}},
     Start_Date => {{ get $v "start-date"}},
     End_Date   => {{ get $v "end-date"}},
     Door_1     => {{ get $v "door 1"}},
     Door_2     => {{ get $v "door 2"}},
     Door_3     => {{ get $v "door 3"}},
     Door_4     => {{ get $v "door 4"}},
     PIN        => {{ get $v "PIN"}}{{ end }}){{ end }}

{{- define "event" }}({{ with $v := .Returns.Value }}
     Index          => {{ get $v "index" }},
     Event          => To_Event_Type ({{ get $v "event type"}}),
     Timestamp      => {{ get $v "timestamp" }},
     Door           => {{ get $v "door" }},
     Direction      => To_Event_Direction ({{ get $v "direction" }}),
     Card           => {{ get $v "card" }},
     Access_Granted => {{ get $v "access granted" }},
     Reason         => To_Event_Reason ({{ get $v "reason" }}){{ end }}){{ end }}

{{- define "time-profile" }}({{ with $v := .Returns.Value }}
     Start_Date => {{ get $v "start date"}},
     End_Date   => {{ get $v "end date"}},
     Weekdays   => (Monday    => {{ get $v "monday" }},
                    Tuesday   => {{ get $v "tuesday" }},
                    Wednesday => {{ get $v "wednesday" }},
                    Thursday  => {{ get $v "thursday" }},
                    Friday    => {{ get $v "friday" }},
                    Saturday  => {{ get $v "saturday" }},
                    Sunday    => {{ get $v "sunday" }}),
     Segments   => [1 => ({{ get $v "segment 1 start"}}, {{ get $v "segment 1 end"}}),
                    2 => ({{ get $v "segment 2 start"}}, {{ get $v "segment 2 end"}}),
                    3 => ({{ get $v "segment 3 start"}}, {{ get $v "segment 3 end"}})],
     Linked_Profile => {{ get $v "linked profile"}}{{ end }}){{ end }}

