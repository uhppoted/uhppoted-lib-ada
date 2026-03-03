with Uhppoted.Lib.Integration_Tests.Stub.Requests;
with Uhppoted.Lib.Integration_Tests.Stub.Replies;

package Uhppoted.Lib.Integration_Tests.Stub.Messages is

   subtype Request is Uhppoted.Lib.Integration_Tests.Stub.Requests.Request;
   subtype Reply      is Uhppoted.Lib.Integration_Tests.Stub.Replies.Reply;
   subtype Reply_List is Uhppoted.Lib.Integration_Tests.Stub.Replies.Reply_List;

   function Get (R : Request) return Reply_List;

end Uhppoted.Lib.Integration_Tests.Stub.Messages;
