with Ada.Strings.Unbounded;

package Uhppoted.Lib.Responses is
   use Ada.Strings.Unbounded;

   --  Message definition for a get-controller response.
   --
   --  @field  Controller  Controller serial number.
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
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_IPv4_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-time response.
   --
   --  @field  Controller  Controller serial number.
   type Get_Time_Response is record
      Controller : Unsigned_32;
      Date_Time  : DateTime;
   end record;

   --  Message definition for a set-time response.
   --
   --  @field  Controller  Controller serial number.
   type Set_Time_Response is record
      Controller : Unsigned_32;
      Date_Time  : DateTime;
   end record;

   --  Message definition for a get-listener response.
   --
   --  @field  Controller  Controller serial number.
   type Get_Listener_Response is record
      Controller : Unsigned_32;
      Address    : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
   end record;

   --  Message definition for a get-listener-addrport response.
   --
   --  @field  Controller  Controller serial number.
   type Get_Listener_Addr_Port_Response is record
      Controller : Unsigned_32;
      Listener   : GNAT.Sockets.Sock_Addr_Type;
      Interval   : Unsigned_8;
   end record;

   --  Message definition for a set-listener response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_Listener_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a set-listener response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_Listener_Addr_Port_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-status response.
   --
   --  @field  Controller  Controller serial number.
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
   --
   --  @field  Controller  Controller serial number.
   type Get_Door_Response is record
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      Open_Delay : Unsigned_8;
   end record;

   --  Message definition for a set-door response.
   --
   --  @field  Controller  Controller serial number.
   type Set_Door_Response is record
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      Open_Delay : Unsigned_8;
   end record;

   --  Message definition for a set-door-passcodes response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_Door_Passcodes_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for an open-door response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Open_Door_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-cards response.
   --
   --  @field  Controller  Controller serial number.
   type Get_Cards_Response is record
      Controller : Unsigned_32;
      Cards      : Unsigned_32;
   end record;

   --  Message definition for a get-card response.
   --
   --  @field  Controller  Controller serial number.
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
   --
   --  @field  Controller  Controller serial number.
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
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Put_Card_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a delete-card response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Delete_Card_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a delete-all-cards response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Delete_All_Cards_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-event response.
   --
   --  @field  Controller  Controller serial number.
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
   --
   --  @field  Controller  Controller serial number.
   type Get_Event_Index_Response is record
      Controller : Unsigned_32;
      Index      : Unsigned_32;
   end record;

   --  Message definition for a set-event-index response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_Event_Index_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a record-special-events response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Record_Special_Events_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-time-profile response.
   --
   --  @field  Controller  Controller serial number.
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
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_Time_Profile_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a clear-time-profiles response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Clear_Time_Profiles_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for an add-task response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Add_Task_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a refresh-tasklist response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Refresh_Task_List_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a clear-tasklist response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Clear_Task_List_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a set-pc-control response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_PC_Control_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a set-interlock response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_Interlock_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for an activate-keypads response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Activate_Keypads_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a get-antipassback response.
   --
   --  @field  Controller  Controller serial number.
   type Get_Antipassback_Response is record
      Controller   : Unsigned_32;
      Antipassback : Unsigned_8;
   end record;

   --  Message definition for a set-antipassback response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_Antipassback_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a set-firstcard response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Set_First_Card_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a restore-default-parameters response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Ok          Success/fail result.
   type Restore_Default_Parameters_Response is record
      Controller : Unsigned_32;
      Ok         : Boolean;
   end record;

   --  Message definition for a listener-event message.
   --
   --  @field  Controller  Controller serial number.
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
