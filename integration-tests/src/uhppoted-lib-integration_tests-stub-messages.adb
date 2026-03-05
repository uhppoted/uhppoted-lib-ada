package body Uhppoted.Lib.Integration_Tests.Stub.Messages is
   use Uhppoted.Lib.Integration_Tests.Stub.Requests;
   use Uhppoted.Lib.Integration_Tests.Stub.Replies;

   None     : constant Reply_List := [];
   Messages : constant Message_List := [
      (Find_Controllers_Request, Find_Controllers_Reply),
      (Get_Controller_Request,   Get_Controller_Reply),
      (Get_Time_Request,         Get_Time_Reply)
   ];

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
