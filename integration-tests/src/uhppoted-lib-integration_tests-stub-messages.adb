package body Uhppoted.Lib.Integration_Tests.Stub.Messages is
   use Uhppoted.Lib.Integration_Tests.Stub.Requests;
   use Uhppoted.Lib.Integration_Tests.Stub.Replies;

   None : constant Reply_List := [];

   function Get (R : Request) return Reply_List is
   begin
      if R = Find_Controllers_Request then
         return Replies.Find_Controllers;
      end if;

      if R = Get_Controller_Request then
         return Replies.Get_Controller;
      end if;

      if R = Get_Time_Request then
         return Replies.Get_Time;
      end if;

      return None;
   end Get;

end Uhppoted.Lib.Integration_Tests.Stub.Messages;
