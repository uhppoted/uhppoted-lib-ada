package body Uhppoted.Lib.Integration_Tests.Stub.Messages is
   use Uhppoted.Lib.Integration_Tests.Stub.Requests;
   use Uhppoted.Lib.Integration_Tests.Stub.Replies;

   Messages : constant Message_List := [
     (Find_Controllers_Request, Find_Controllers_Reply),
     (Get_Controller_Request,   Get_Controller_Reply),
     (Set_IPv4_Request,         Set_IPv4_Reply),
     (Get_Time_Request,         Get_Time_Reply),
     (Set_Time_Request,         Set_Time_Reply)
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
