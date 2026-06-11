with Uhppoted.Types;
with Uhppoted.Lib.Types;

--  Codec functions to encode a request message.
--

package Uhppoted.Lib.Encode is

   --  Encodes a get-controller request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Controller (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-IPv4 request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Addr        Controller IPv4 address.
   --  @param  Netmask     Controller IPv4 subnet mask.
   --  @param  Gateway     Controller IPv4 gateway address.
   --
   --  @return  Packet  64 byte message packet.
   function Set_IPv4
     (Controller : Unsigned_32;
      Addr       : GNAT.Sockets.Inet_Addr_Type;
      Netmask    : GNAT.Sockets.Inet_Addr_Type;
      Gateway    : GNAT.Sockets.Inet_Addr_Type) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-time request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Time (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-time request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  DT          Date/time (yyyy-mm-dd HH:mm).
   --
   --  @return  Packet  64 byte message packet.
   function Set_Time (Controller : Unsigned_32; DT : Uhppoted.Types.DateTime) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-listener request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Listener (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-listener request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Listener_Addr_Port (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-listener request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Addr        Listener IPv4 address.
   --  @param  Port        Listener port [1..65535].
   --  @param  Interval    Interval (seconds) at which to automatically send controller state (0 for none).
   --
   --  @return  Packet  64 byte message packet.
   --!format off
   function Set_Listener (Controller : Unsigned_32;
                          Addr       : GNAT.Sockets.Inet_Addr_Type;
                          Port       : Unsigned_16;
                          Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet;
   --!format on

   --  Encodes a set-listener request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Listener    Listener IPv4 address:port.
   --  @param  Interval    Interval (seconds) at which to automatically send controller state (0 for none).
   --
   --  @return  Packet  64 byte message packet.
   --!format off
   function Set_Listener_Addr_Port
      (Controller : Unsigned_32;
       Listener   : GNAT.Sockets.Sock_Addr_Type;
       Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet;
   --!format on

   --  Encodes a get-status request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Status (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-door request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Door        Door ID [1..4].
   --
   --  @return  Packet  64 byte message packet.
   function Get_Door (Controller : Unsigned_32; Door : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-door request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Door        Door ID [1..4].
   --  @param  Mode        Door control mode (1:normally open, 2:normally close, 3: controlled).
   --  @param  OpenDelay   Door unlock duration (seconds).
   --
   --  @return  Packet  64 byte message packet.
   --!format off
   function Set_Door
      (Controller : Unsigned_32;
       Door       : Unsigned_8;
       Mode       : Uhppoted.Lib.Control_Mode;
       OpenDelay  : Unsigned_8) return Uhppoted.Lib.Types.Packet;
   --!format on

   --  Encodes a set-door-passcodes request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Door        Door ID [1..4].
   --  @param  Passcode1   First passcode [0..999999] (0 for none).
   --  @param  Passcode2   Second passcode [0..999999] (0 for none).
   --  @param  Passcode3   Third passcode [0..999999] (0 for none).
   --  @param  Passcode4   Fourth passcode [0..999999] (0 for none).
   --
   --  @return  Packet  64 byte message packet.
   function Set_Door_Passcodes
     (Controller : Unsigned_32;
      Door       : Unsigned_8;
      Passcode1  : Unsigned_32;
      Passcode2  : Unsigned_32;
      Passcode3  : Unsigned_32;
      Passcode4  : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes an open-door request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Door        Door ID [1..4].
   --
   --  @return  Packet  64 byte message packet.
   function Open_Door (Controller : Unsigned_32; Door : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-cards request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-card request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Card        Card number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-card-by-index request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Index       Card record index.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Card_At_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a put-card request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Card        Card number.
   --  @param  Start_Date  Date from which card is valid.
   --  @param  End_Date    Date after which card is no longer valid.
   --  @param  Door_1      Access permissions for door 1 (0: none, 1:24x7, 2..254: time profile).
   --  @param  Door_2      Access permissions for door 2 (0: none, 1:24x7, 2..254: time profile).
   --  @param  Door_3      Access permissions for door 3 (0: none, 1:24x7, 2..254: time profile).
   --  @param  Door_4      Access permissions for door 4 (0: none, 1:24x7, 2..254: time profile).
   --  @param  PIN         Access reader PIN code [0..999999] (0 for none).
   --
   --  @return  Packet  64 byte message packet.
   function Put_Card
     (Controller : Unsigned_32;
      Card       : Unsigned_32;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_24) return Uhppoted.Lib.Types.Packet;

   --  Encodes a delete-card request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Card        Card number.
   --
   --  @return  Packet  64 byte message packet.
   function Delete_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a delete-cards request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Delete_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-event request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Index       Event index.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Event (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-event-index request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Event_Index (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-event-index request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Index       Event index.
   --
   --  @return  Packet  64 byte message packet.
   function Set_Event_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a record-special-events request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Enabled     Enables/disables door/pusbutton/etc events.
   --
   --  @return  Packet  64 byte message packet.
   function Record_Special_Events (Controller : Unsigned_32; Enabled : Boolean) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-time-profile request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Profile     Time profile ID [2..254].
   --
   --  @return  Packet  64 byte message packet.
   function Get_Time_Profile (Controller : Unsigned_32; Profile : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-time-profile request as a 64 byte array.
   --
   --  @param  Controller       Controller serial number.
   --  @param  Profile          Time profile ID [2..254].
   --  @param  Start_Date       Date from which time profile is enabled.
   --  @param  End_Date         Date after which time profile is no longer enabled.
   --  @param  Monday           Enables/disables time profile on Monday.
   --  @param  Tuesday          Enables/disables time profile on Tuesday.
   --  @param  Wednesday        Enables/disables time profile on Wednesday.
   --  @param  Thursday         Enables/disables time profile on Thursday.
   --  @param  Friday           Enables/disables time profile on Friday.
   --  @param  Saturday         Enables/disables time profile on Saturday.
   --  @param  Sunday           Enables/disables time profile on Sunday.
   --  @param  Segment_1_Start  Hour of day for start of first time segment.
   --  @param  Segment_1_End    Hour of day for end of first time segment.
   --  @param  Segment_2_Start  Hour of day for start of second time segment.
   --  @param  Segment_2_End    Hour of day for end of second time segment.
   --  @param  Segment_3_Start  Hour of day for start of third time segment.
   --  @param  Segment_3_End    Hour of day for end of third time segment.
   --  @param  Linked_Profile   Profile ID [2..254] of time profile with additional
   --                           constraints/segments (0 if none).
   --
   --  @return  Packet  64 byte message packet.
   function Set_Time_Profile
     (Controller      : Unsigned_32;
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
      Linked_Profile  : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a clear-time-profiles request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Clear_Time_Profiles (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes an add-task request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Task_ID     Scheduled task type.
   --  @param  Start_Date  Date from which scheduled task is enabled.
   --  @param  End_Date    Date after which scheduled task is no longer enabled.
   --  @param  Monday      Enables/disables scheduled task on Monday.
   --  @param  Tuesday     Enables/disables scheduled task on Tuesday.
   --  @param  Wednesday   Enables/disables scheduled task on Wednesday.
   --  @param  Thursday    Enables/disables scheduled task on Thursday.
   --  @param  Friday      Enables/disables scheduled task on Friday.
   --  @param  Saturday    Enables/disables scheduled task on Saturday.
   --  @param  Sunday      Enables/disables scheduled task on Sunday.
   --  @param  Start_Time  Hour of day at which scheduled task is run.
   --  @param  Door        Door ID [1..4] for scheduled task action.
   --  @param  More_Cards  Number of 'more cards' for More_Cards task ID.
   --
   --  @return  Packet  64 byte message packet.
   function Add_Task
     (Controller : Unsigned_32;
      Task_ID    : Task_Type;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Monday     : Boolean;
      Tuesday    : Boolean;
      Wednesday  : Boolean;
      Thursday   : Boolean;
      Friday     : Boolean;
      Saturday   : Boolean;
      Sunday     : Boolean;
      Start_Time : HHmm;
      Door       : Unsigned_8;
      More_Cards : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a refresh-tasklist request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Refresh_Task_List (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a clear-tasklist request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Clear_Task_List (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-pc-control request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Enable      Enables/disables remote access control.
   --
   --  @return  Packet  64 byte message packet.
   function Set_PC_Control (Controller : Unsigned_32; Enable : Boolean) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-interlock request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Interlock   Door interlock mode.
   --
   --  @return  Packet  64 byte message packet.
   function Set_Interlock
     (Controller : Unsigned_32;
      Interlock  : Uhppoted.Lib.Interlock) return Uhppoted.Lib.Types.Packet;

   --  Encodes an activate-keypads request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --  @param  Reader_1    Enables/disables reader 1.
   --  @param  Reader_2    Enables/disables reader 2.
   --  @param  Reader_3    Enables/disables reader 3.
   --  @param  Reader_4    Enables/disables reader 4.
   --
   --  @return  Packet  64 byte message packet.
   function Activate_Keypads
     (Controller : Unsigned_32;
      Reader_1   : Boolean;
      Reader_2   : Boolean;
      Reader_3   : Boolean;
      Reader_4   : Boolean) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-antipassback request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Get_Antipassback (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-antipassback request as a 64 byte array.
   --
   --  @param  Controller    Controller serial number.
   --  @param  Antipassback  Anti-passback mode.
   --
   --  @return  Packet  64 byte message packet.
   function Set_Antipassback
     (Controller   : Unsigned_32;
      Antipassback : Uhppoted.Lib.Antipassback) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-firstcard request as a 64 byte array.
   --
   --  @param  Controller     Controller serial number.
   --  @param  Door           Door ID [1..4].
   --  @param  Start_Time     Time of day (HH:mm) from which first-card mode can be activated.
   --  @param  End_Time       Time of day (HH:mm) after which first-card mode is deactivated.
   --  @param  Active_Mode    Door control mode after first-card swipe (0:controlled, 1:normally open,
   --                         2:normally closed).
   --  @param  Inactive_Mode  Door control mode when first card mode is deactivated (0:controlled, 1:normally open,
   --                         2:normally closed, 3:first-card only).
   --  @param  Monday         Enables/disables first-card mode on Monday.
   --  @param  Tuesday        Enables/disables first-card mode on Tuesday.
   --  @param  Wednesday      Enables/disables first-card mode on Wednesday.
   --  @param  Thursday       Enables/disables first-card mode on Thursday.
   --  @param  Friday         Enables/disables first-card mode on Friday.
   --  @param  Saturday       Enables/disables first-card mode on Saturday.
   --  @param  Sunday         Enables/disables first-card mode on Sunday.
   --
   --  @return  Packet  64 byte message packet.
   function Set_First_Card
     (Controller    : Unsigned_32;
      Door          : Unsigned_8;
      Start_Time    : HHmm;
      End_Time      : HHmm;
      Active_Mode   : Control_Mode;
      Inactive_Mode : Control_Mode;
      Monday        : Boolean;
      Tuesday       : Boolean;
      Wednesday     : Boolean;
      Thursday      : Boolean;
      Friday        : Boolean;
      Saturday      : Boolean;
      Sunday        : Boolean) return Uhppoted.Lib.Types.Packet;

   --  Encodes a restore-default-parameters request as a 64 byte array.
   --
   --  @param  Controller  Controller serial number.
   --
   --  @return  Packet  64 byte message packet.
   function Restore_Default_Parameters (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

end Uhppoted.Lib.Encode;
