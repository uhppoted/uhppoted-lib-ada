with Ada.Strings.Unbounded;

package Uhppoted.Lib.Responses is
   use Ada.Strings.Unbounded;

   --  Message definition for a get-controller response.
   type Get_Controller_Response is record
      Controller  : Unsigned_32;
      IP_Address  : IPv4;
      Subnet_Mask : IPv4;
      Gateway     : IPv4;
      MAC_Address : Hardware_Addr;
      Version     : Unbounded_String;
      Date        : DateOnly;
   end record;

   --  Message definition for a set-IPv4 response.
   type Set_IPv4_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-time response.
   type Get_Time_Response is record
      Controller : Unsigned_32;
      Date_Time  : DateTime;
   end record;

   --  Message definition for a set-time response.
   type Set_Time_Response is record
      Controller : Unsigned_32;
      Date_Time  : DateTime;
   end record;

   --  Message definition for a get-listener response.
   type Get_Listener_Response is record
      Controller : Unsigned_32;
      Address    : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
   end record;

   --  Message definition for a get-listener-addrport response.
   type Get_Listener_Addr_Port_Response is record
      Controller : Unsigned_32;
      Listener   : GNAT.Sockets.Sock_Addr_Type;
      Interval   : Unsigned_8;
   end record;

   --  Message definition for a set-listener response.
   type Set_Listener_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a set-listener response.
   type Set_Listener_Addr_Port_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-status response.
   type Relay_State is mod 256;
   type Inputs_State is mod 256;

   type Get_Status_Response is record
      Controller           : Unsigned_32;
      System_Date          : DateOnly;
      System_Time          : TimeOnly;
      Door_1_Open          : Boolean;
      Door_2_Open          : Boolean;
      Door_3_Open          : Boolean;
      Door_4_Open          : Boolean;
      Door_1_Button        : Boolean;
      Door_2_Button        : Boolean;
      Door_3_Button        : Boolean;
      Door_4_Button        : Boolean;
      Relays               : Relay_State;
      Inputs               : Inputs_State;
      System_Error         : Unsigned_8;
      Special_Info         : Unsigned_8;
      Event_Index          : Unsigned_32;
      Event_Type           : Unsigned_8;
      Event_Access_Granted : Boolean;
      Event_Door           : Unsigned_8;
      Event_Direction      : Unsigned_8;
      Event_Card           : Unsigned_32;
      Event_Timestamp      : DateTime;
      Event_Reason         : Unsigned_8;
      Sequence_No          : Unsigned_32;
   end record;

   --  Message definition for a get-door response.
   type Get_Door_Response is record
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      OpenDelay  : Unsigned_8;
   end record;

   --  Message definition for a set-door response.
   type Set_Door_Response is record
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      OpenDelay  : Unsigned_8;
   end record;

   --  Message definition for a set-door-passcodes response.
   type Set_Door_Passcodes_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for an open-door response.
   type Open_Door_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-cards response.
   type Get_Cards_Response is record
      Controller : Unsigned_32;
      Cards      : Unsigned_32;
   end record;

   --  Message definition for a get-card response.
   type Get_Card_Response is record
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_24;
   end record;

   --  Message definition for a get-card-at-index response.
   type Get_Card_At_Index_Response is record
      Controller : Unsigned_32;
      Card       : Unsigned_32;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_24;
   end record;

   --  Message definition for a put-card response.
   type Put_Card_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a delete-card response.
   type Delete_Card_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a delete-all-cards response.
   type Delete_All_Cards_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-event response.
   type Get_Event_Response is record
      Controller     : Unsigned_32;
      Index          : Unsigned_32;
      Event_Type     : Unsigned_8;
      Access_Granted : Boolean;
      Door           : Unsigned_8;
      Direction      : Unsigned_8;
      Card           : Unsigned_32;
      Timestamp      : DateTime;
      Reason         : Unsigned_8;
   end record;

   --  Message definition for a get-event-index response.
   type Get_Event_Index_Response is record
      Controller : Unsigned_32;
      Index      : Unsigned_32;
   end record;

   --  Message definition for a set-event-index response.
   type Set_Event_Index_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a record-special-events response.
   type Record_Special_Events_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-time-profile response.
   type Get_Time_Profile_Response is record
      Controller      : Unsigned_32;
      Profile         : Unsigned_8;
      Start_Date      : DateOnly;
      End_Date        : DateOnly;
      Monday          : Boolean;
      Tuesday         : Boolean;
      Wednesday       : Boolean;
      Thursday        : Boolean;
      Friday          : Boolean;
      Saturday        : Boolean;
      Sunday          : Boolean;
      Segment_1_Start : HHmm;
      Segment_1_End   : HHmm;
      Segment_2_Start : HHmm;
      Segment_2_End   : HHmm;
      Segment_3_Start : HHmm;
      Segment_3_End   : HHmm;
      Linked_Profile  : Unsigned_8;
   end record;

   --  Message definition for a set-time-profile response.
   type Set_Time_Profile_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a clear-time-profiles response.
   type Clear_Time_Profiles_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for an add-task response.
   type Add_Task_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a refresh-tasklist response.
   type Refresh_Task_List_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a clear-tasklist response.
   type Clear_Task_List_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a set-pc-control response.
   type Set_PC_Control_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a set-interlock response.
   type Set_Interlock_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for an activate-keypads response.
   type Activate_Keypads_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-antipassback response.
   type Get_Antipassback_Response is record
      Controller   : Unsigned_32;
      Antipassback : Unsigned_8;
   end record;

   --  Message definition for a set-antipassback response.
   type Set_Antipassback_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a set-firstcard response.
   type Set_First_Card_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a restore-default-parameters response.
   type Restore_Default_Parameters_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a listener-event message.
   type Listener_Event is record
      Controller           : Unsigned_32;
      System_Date          : DateOnly;
      System_Time          : TimeOnly;
      Door_1_Open          : Boolean;
      Door_2_Open          : Boolean;
      Door_3_Open          : Boolean;
      Door_4_Open          : Boolean;
      Door_1_Button        : Boolean;
      Door_2_Button        : Boolean;
      Door_3_Button        : Boolean;
      Door_4_Button        : Boolean;
      Relays               : Relay_State;
      Inputs               : Inputs_State;
      System_Error         : Unsigned_8;
      Special_Info         : Unsigned_8;
      Event_Index          : Unsigned_32;
      Event_Type           : Unsigned_8;
      Event_Access_Granted : Boolean;
      Event_Door           : Unsigned_8;
      Event_Direction      : Unsigned_8;
      Event_Card           : Unsigned_32;
      Event_Timestamp      : DateTime;
      Event_Reason         : Unsigned_8;
      Sequence_No          : Unsigned_32;
   end record;

end Uhppoted.Lib.Responses;
