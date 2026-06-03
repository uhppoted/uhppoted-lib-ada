with Ada.Strings.Unbounded;

--  Internal record definitions for the response to commands and requests.
--
package Uhppoted.Lib.Responses is
   use Ada.Strings.Unbounded;

   --  (weird gnatdoc bug: blank lines above and below required or param/field tags are rejected)

   --  Message definition for a get-controller response.
   --
   --  @field  Controller   Controller serial number.
   --  @field  IP_Address   Controller IPv4 address.
   --  @field  Subnet_Mask  IPv4 subnet mask.
   --  @field  Gateway      IPv4 gateway address.
   --  @field  MAC_Address  Controller MAC address.
   --  @field  Version      Firmware version.
   --  @field  Date         Firmware release date.
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
   --  @field  Date_Time   Controller current date and time.
   type Get_Time_Response is record
      Controller : Unsigned_32;
      Date_Time  : DateTime;
   end record;

   --  Message definition for a set-time response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Date_Time   Date and time to set.
   type Set_Time_Response is record
      Controller : Unsigned_32;
      Date_Time  : DateTime;
   end record;

   --  Message definition for a get-listener response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Address     Configured event listener IPv4 address.
   --  @field  Port        Configured event listener port.
   --  @field  Interval    Interval (seconds) at which controller state is automatically
   --                      sent to configured listener (0 for on-event only).
   type Get_Listener_Response is record
      Controller : Unsigned_32;
      Address    : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
   end record;

   --  Message definition for a get-listener-addrport response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Listener    Configured event listener IPv4 address:port.
   --  @field  Interval    Interval (seconds) at which controller state is automatically
   --                      sent to configured listener (0 for on-event only).
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

   --  Convenience type for unlock relays state bitset.
   type Relay_State is mod 256;

   --  Convenience type for inputs state bitset.
   type Inputs_State is mod 256;

   --  Message definition for a get-status response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  System_Date          Controller current date.
   --  @field  System_Time          Controller current time.
   --  @field  Door_1_Open          Door 1 open sensor state.
   --  @field  Door_2_Open          Door 2 open sensor state.
   --  @field  Door_3_Open          Door 3 open sensor state.
   --  @field  Door_4_Open          Door 4 open sensor state.
   --  @field  Door_1_Button        Door 1 push button state.
   --  @field  Door_2_Button        Door 2 push button state.
   --  @field  Door_3_Button        Door 3 push button state.
   --  @field  Door_4_Button        Door 4 push button state.
   --  @field  Relays               Door unlock relay state bitset.
   --  @field  Inputs               Alarm inputs state bitset.
   --  @field  System_Error         System error code (0 for none).
   --  @field  Special_Info         Not a clue.
   --  @field  Event_Index          Index of most recent event (0 for none).
   --  @field  Event_Type           Type of most recent event (if any).
   --  @field  Event_Access_Granted True if access was allowed.
   --  @field  Event_Door           Door ID [1..4] for a door event.
   --  @field  Event_Direction      Direction for a door event.
   --  @field  Event_Card           Card number for a card event.
   --  @field  Event_Timestamp      Event timestamp.
   --  @field  Event_Reason         Event reason code.
   --  @field  Sequence_No          Presumably some kind of incrementing message ID.
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
   --  @field  Door        Door ID [1..4].
   --  @field  Mode        Door control mode (Controlled, Normally_Open or Normally_Closed).
   --  @field  Open_Delay  Unlock duration (seconds).
   type Get_Door_Response is record
      Controller : Unsigned_32;
      Door       : Unsigned_8;
      Mode       : Unsigned_8;
      Open_Delay : Unsigned_8;
   end record;

   --  Message definition for a set-door response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Door        Door ID [1..4].
   --  @field  Mode        Door control mode (Controlled, Normally_Open or Normally_Closed).
   --  @field  Open_Delay  Unlock duration (seconds).
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
   --  @field  Cards       Number of stored cards.
   type Get_Cards_Response is record
      Controller : Unsigned_32;
      Cards      : Unsigned_32;
   end record;

   --  Message definition for a get-card response.
   --
   --  @field  Controller  Controller serial number.
   --  @field  Card        Card number.
   --  @field  Start_Date  Date from which card is valid.
   --  @field  End_Date    Date after which card is no longer valid.
   --  @field  Door_1      Door 1 access permissions (0: none, 1: 24x7, 2..254: access time profile).
   --  @field  Door_2      Door 2 access permissions (0: none, 1: 24x7, 2..254: access time profile).
   --  @field  Door_3      Door 3 access permissions (0: none, 1: 24x7, 2..254: access time profile).
   --  @field  Door_4      Door 4 access permissions (0: none, 1: 24x7, 2..254: access time profile).
   --  @field  PIN         Keypad PIN code [0..999999] (0 for none).
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
   --  @field  Card        Card number.
   --  @field  Start_Date  Date from which card is valid.
   --  @field  End_Date    Date after which card is no longer valid.
   --  @field  Door_1      Door 1 access permissions (0: none, 1: 24x7, 2..254: access time profile).
   --  @field  Door_2      Door 2 access permissions (0: none, 1: 24x7, 2..254: access time profile).
   --  @field  Door_3      Door 3 access permissions (0: none, 1: 24x7, 2..254: access time profile).
   --  @field  Door_4      Door 4 access permissions (0: none, 1: 24x7, 2..254: access time profile).
   --  @field  PIN         Keypad PIN code [0..999999] (0 for none).
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
   --  @field  Controller     Controller serial number.
   --  @field  Index          Event record index.
   --  @field  Event_Type     Event type code.
   --  @field  Access_Granted True if access was allowed.
   --  @field  Door           Door ID [1..4] for door/access events.
   --  @field  Direction      Direction for door/access events.
   --  @field  Card           Card number for swipe/access events.
   --  @field  Timestamp      Event timestamp.
   --  @field  Reason         Event reason code.
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
   --  @field  Index       Downloaded event index.
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
   --  @field  Controller      Controller serial number.
   --  @field  Profile         Time profile ID [2..254].
   --  @field  Start_Date      Date from which time profile is enabled.
   --  @field  End_Date        Date after which time profile is no longer enabled.
   --  @field  Monday          True if time profile is enabled on Mondays.
   --  @field  Tuesday         True if time profile is enabled on Tuesdays.
   --  @field  Wednesday       True if time profile is enabled on Wednesdays.
   --  @field  Thursday        True if time profile is enabled on Thursdays.
   --  @field  Friday          True if time profile is enabled on Fridays.
   --  @field  Saturday        True if time profile is enabled on Saturdays.
   --  @field  Sunday          True if time profile is enabled on Sundays.
   --  @field  Segment_1_Start Time segment 1 start time.
   --  @field  Segment_1_End   Time segment 1 end time.
   --  @field  Segment_2_Start Time segment 2 start time.
   --  @field  Segment_2_End   Time segment 2 end time.
   --  @field  Segment_3_Start Time segment 3 start time.
   --  @field  Segment_3_End   Time segment 3 end time.
   --  @field  Linked_Profile  Linked time profile with additional constraints/segments (0 for none).
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
   --  @field  Controller    Controller serial number.
   --  @field  Antipassback  Controller anti-passback setting.
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
   --  @field  Controller           Controller serial number.
   --  @field  System_Date          Controller current date.
   --  @field  System_Time          Controller current time.
   --  @field  Door_1_Open          Door 1 open sensor state.
   --  @field  Door_2_Open          Door 2 open sensor state.
   --  @field  Door_3_Open          Door 3 open sensor state.
   --  @field  Door_4_Open          Door 4 open sensor state.
   --  @field  Door_1_Button        Door 1 push button state.
   --  @field  Door_2_Button        Door 2 push button state.
   --  @field  Door_3_Button        Door 3 push button state.
   --  @field  Door_4_Button        Door 4 push button state.
   --  @field  Relays               Door unlock relay state bitset.
   --  @field  Inputs               Alarm inputs state bitset.
   --  @field  System_Error         System error code (0 for none).
   --  @field  Special_Info         Not a clue.
   --  @field  Event_Index          Index of most recent event (0 for none).
   --  @field  Event_Type           Type of most recent event (if any).
   --  @field  Event_Access_Granted True if access was allowed.
   --  @field  Event_Door           Door ID [1..4] for a door event.
   --  @field  Event_Direction      Direction for a door event.
   --  @field  Event_Card           Card number for a card event.
   --  @field  Event_Timestamp      Event timestamp.
   --  @field  Event_Reason         Event reason code.
   --  @field  Sequence_No          Presumably some kind of incrementing message ID.
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
