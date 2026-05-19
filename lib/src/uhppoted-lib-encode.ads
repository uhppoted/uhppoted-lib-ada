with Uhppoted.Types;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Encode is

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-IPv4 request as a 64 byte array.
   function Set_IPv4 (Controller : Unsigned_32;
                      Addr       : GNAT.Sockets.Inet_Addr_Type;
                      Netmask    : GNAT.Sockets.Inet_Addr_Type;
                      Gateway    : GNAT.Sockets.Inet_Addr_Type) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-time request as a 64 byte array.
   function Get_Time (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-time request as a 64 byte array.
   function Set_Time (Controller : Unsigned_32;
                      DT         : Uhppoted.Types.DateTime) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener_Addr_Port (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-listener request as a 64 byte array.
   function Set_Listener (Controller : Unsigned_32;
                          Addr       : GNAT.Sockets.Inet_Addr_Type;
                          Port       : Unsigned_16;
                          Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-listener request as a 64 byte array.
   function Set_Listener_Addr_Port (Controller : Unsigned_32;
                                    Listener   : GNAT.Sockets.Sock_Addr_Type;
                                    Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-status request as a 64 byte array.
   function Get_Status (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-door request as a 64 byte array.
   function Get_Door (Controller : Unsigned_32; Door : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-door request as a 64 byte array.
   function Set_Door (Controller : Unsigned_32;
                      Door       : Unsigned_8;
                      Mode       : Uhppoted.Lib.Control_Mode;
                      OpenDelay  : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-door-passcodes request as a 64 byte array.
   function Set_Door_Passcodes (Controller : Unsigned_32;
                                Door       : Unsigned_8;
                                Passcode1  : Unsigned_32;
                                Passcode2  : Unsigned_32;
                                Passcode3  : Unsigned_32;
                                Passcode4  : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes an open-door request as a 64 byte array.
   function Open_Door (Controller : Unsigned_32;
                       Door       : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-cards request as a 64 byte array.
   function Get_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-card request as a 64 byte array.
   function Get_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-card-by-index request as a 64 byte array.
   function Get_Card_At_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a put-card request as a 64 byte array.
   function Put_Card (Controller : Unsigned_32;
                      Card       : Unsigned_32;
                      Start_Date : DateOnly;
                      End_Date   : DateOnly;
                      Door_1     : Unsigned_8;
                      Door_2     : Unsigned_8;
                      Door_3     : Unsigned_8;
                      Door_4     : Unsigned_8;
                      PIN        : Unsigned_24) return Uhppoted.Lib.Types.Packet;

   --  Encodes a delete-card request as a 64 byte array.
   function Delete_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a delete-cards request as a 64 byte array.
   function Delete_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-event request as a 64 byte array.
   function Get_Event (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-event-index request as a 64 byte array.
   function Get_Event_Index (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-event-index request as a 64 byte array.
   function Set_Event_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a record-special-events request as a 64 byte array.
   function Record_Special_Events (Controller : Unsigned_32; Enabled : Boolean) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-time-profile request as a 64 byte array.
   function Get_Time_Profile (Controller : Unsigned_32; Profile : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-time-profile request as a 64 byte array.
   function Set_Time_Profile (Controller      : Unsigned_32;
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
   function Clear_Time_Profiles (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes an add-task request as a 64 byte array.
   function Add_Task (Controller : Unsigned_32;
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
   function Refresh_Task_List (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a clear-tasklist request as a 64 byte array.
   function Clear_Task_List (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-pc-control request as a 64 byte array.
   function Set_PC_Control (Controller : Unsigned_32; Enable : Boolean) return Uhppoted.Lib.Types.Packet;

   --  Encodes a restore-default-parameters request as a 64 byte array.
   function Restore_Default_Parameters (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

end Uhppoted.Lib.Encode;
