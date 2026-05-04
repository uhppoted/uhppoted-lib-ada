with Ada.Containers.Vectors;

package Uhppoted.Lib.Integration_Tests.Stub.Events is

   type Message is array (1 .. 64) of Interfaces.Unsigned_8;

   type Event is record
      Msg   : Message;
      State : Uhppoted.Lib.Controller_State;
      Event : Uhppoted.Lib.Controller_Event;
   end record;

   package Events_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Event);

   subtype Events_List is Events_Vector.Vector;
{{ range $test := .Tests }}
{{- template "listen-message" $test }}{{end}}
{{- range $test := .Tests }}
{{- template "listen-state" $test }}{{end}}
{{- range $test := .Tests }}
{{- template "listen-event" $test }}{{end}}
   Events : constant Events_List := [
{{- range $ix,$test := .Tests }}{{if $ix}},{{end}}
      ({{ printf "%v_Message," .Name | rpad 29}} {{ printf "%v_State," .Name | rpad 27}} {{ printf "%v_Event" .Name}}){{- end }}
   ];

end Uhppoted.Lib.Integration_Tests.Stub.Events;
{{- define "listen-message"}}
   {{ .Name }}_Message : constant Message := [
      {{- range $bytes := .Message }}
      {{ $bytes }}{{ end }}
   ];
{{ end }}
{{- define "listen-state"}}
   {{ .Name }}_State : constant Controller_State := (
      System_Date_Time => {{ index .Expected "system-date-time" }},
      Doors => [1 => (Open     => {{ index .Expected "door-1-open"     }},
                      Button   => {{ index .Expected "door-1-button"   }},
                      Unlocked => {{ index .Expected "door-1-unlocked" }}),
                2 => (Open     => {{ index .Expected "door-2-open"     }},
                      Button   => {{ index .Expected "door-2-button"   }},
                      Unlocked => {{ index .Expected "door-2-unlocked" }}),
                3 => (Open     => {{ index .Expected "door-3-open"     }},
                      Button   => {{ index .Expected "door-3-button"   }},
                      Unlocked => {{ index .Expected "door-3-unlocked" }}),
                4 => (Open     => {{ index .Expected "door-4-open"     }},
                      Button   => {{ index .Expected "door-4-button"   }},
                      Unlocked => {{ index .Expected "door-4-unlocked" }})],
      Alarms => (Flags       => {{ index .Expected "alarm-flags"  }},
                 Fire        => {{ index .Expected "alarm-fire"   }},
                 Lock_Forced => {{ index .Expected "alarm-forced" }}),
      System_Error => {{ index .Expected "system-error" }},
      Special_Info => {{ index .Expected "special-info" }});
{{ end }}
{{- define "listen-event"}}
   {{ .Name }}_Event : constant Controller_Event := (
      Index          => {{ index .Expected "event-index"     }},
      Event          => {{ index .Expected "event-event"     }},
      Timestamp      => {{ index .Expected "event-timestamp" }},
      Door           => {{ index .Expected "event-door"      }},
      Direction      => {{ index .Expected "event-direction" }},
      Card           => {{ index .Expected "event-card"      }},
      Access_Granted => {{ index .Expected "event-granted"   }},
      Reason         => {{ index .Expected "event-reason"    }});
{{ end }}
