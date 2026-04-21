package body Uhppoted.Lib.Integration_Tests.Stub.Messages is
   use Uhppoted.Lib.Integration_Tests.Stub.Requests;
   use Uhppoted.Lib.Integration_Tests.Stub.Replies;

   Messages : constant Message_List := [
     (Find_Controllers_Request,            Find_Controllers_Reply),
     (Get_Controller_Request,              Get_Controller_Reply),
     (Set_IPv4_Request,                    Set_IPv4_Reply),
     (Get_Time_Request,                    Get_Time_Reply),
     (Set_Time_Request,                    Set_Time_Reply),
     (Get_Listener_Request,                Get_Listener_Reply),
     (Set_Listener_Request,                Set_Listener_Reply),
     (Get_Status_Request,                  Get_Status_Reply),
     (Get_Status_No_Event_Request,         Get_Status_No_Event_Reply),
     (Get_Door_Request,                    Get_Door_Reply),
     (Set_Door_Request,                    Set_Door_Reply),
     (Set_Door_Passcodes_Request,          Set_Door_Passcodes_Reply),
     (Open_Door_Request,                   Open_Door_Reply),
     (Get_Cards_Request,                   Get_Cards_Reply),
     (Get_Card_Request,                    Get_Card_Reply),
     (Get_Card_Not_Found_Request,          Get_Card_Not_Found_Reply),
     (Get_Card_At_Index_Request,           Get_Card_At_Index_Reply),
     (Get_Card_At_Index_Not_Found_Request, Get_Card_At_Index_Not_Found_Reply),
     (Get_Card_At_Index_Deleted_Request,   Get_Card_At_Index_Deleted_Reply),
     (Put_Card_Request,                    Put_Card_Reply),
     (Delete_Card_Request,                 Delete_Card_Reply),
     (Delete_All_Cards_Request,            Delete_All_Cards_Reply),
     (Get_Event_Index_Request,             Get_Event_Index_Reply),
     (Set_Event_Index_Request,             Set_Event_Index_Reply),
     (Record_Special_Events_Request,       Record_Special_Events_Reply),
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
