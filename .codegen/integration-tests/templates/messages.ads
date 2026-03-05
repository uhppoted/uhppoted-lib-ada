with Ada.Containers.Vectors;
with Uhppoted.Lib.Integration_Tests.Stub.Requests;
with Uhppoted.Lib.Integration_Tests.Stub.Replies;

package Uhppoted.Lib.Integration_Tests.Stub.Messages is

   subtype Request    is Uhppoted.Lib.Integration_Tests.Stub.Requests.Request;
   subtype Reply      is Uhppoted.Lib.Integration_Tests.Stub.Replies.Reply;
   subtype Reply_List is Uhppoted.Lib.Integration_Tests.Stub.Replies.Reply_List;

   function Get (R : Request) return Reply_List;

private
   type Message is record
      RQ    : Request;
      Reply : Reply_List;
   end record;

   package Message_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Message);

   subtype Message_List is Message_Vectors.Vector;

end Uhppoted.Lib.Integration_Tests.Stub.Messages;
