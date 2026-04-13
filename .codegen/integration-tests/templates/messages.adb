package body Uhppoted.Lib.Integration_Tests.Stub.Messages is
   use Uhppoted.Lib.Integration_Tests.Stub.Requests;
   use Uhppoted.Lib.Integration_Tests.Stub.Replies;

   Messages : constant Message_List := [
{{- range $ix,$test := .Tests }}
{{- template "message" $test }},
{{- end }}
     (Invalid_SOM_Request,                 Invalid_SOM_Reply),
     (Invalid_OpCode_Request,              Invalid_OpCode_Reply)
   ];

   None : constant Reply_List := [];

   function Get (R : Request) return Reply_List is
   begin
      for Msg of Messages loop
         if Msg.RQ = R then
            return Msg.Reply;
         end if;
      end loop;

      return None;
   end Get;

end Uhppoted.Lib.Integration_Tests.Stub.Messages;
{{- define "message"}}
     ({{ printf "%v_Request," .Name | rpad 36 }} {{ .Name }}_Reply)
{{- end }}
