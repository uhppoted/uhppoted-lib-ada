with Interfaces;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with GNAT.Sockets;

package Uhppoted.Types is
   use Ada.Strings.Unbounded;
   use Interfaces;

   --  Custom exception for invalid destination address errors.
   Invalid_Address_Error : exception;

   --  Custom exception for invalid door control mode errors.
   Invalid_Door_Mode_Error : exception;

   --  Custom exception for card not found errors.
   Card_Not_Found_Error : exception;

   --  Custom exception for card deleted errors.
   Card_Deleted_Error : exception;

   --  Custom exception for event not found errors.
   Event_Not_Found_Error : exception;

   --  Custom exception for event overwritten errors.
   Event_Overwritten_Error : exception;

   --  Custom exception for time profile not found errors.
   Time_Profile_Not_Found_Error : exception;

   --  Custom exception for invalid response errors.
   Invalid_Response_Error : exception;

   --  Custom exception for timeout errors.
   Timeout_Error : exception;

   type IPv4 is array (1 .. 4) of Interfaces.Unsigned_8;
   type Hardware_Addr is array (1 .. 6) of Unsigned_8;
   subtype Firmware_Version is String (1 .. 5);

   type DateOnly is record
      Year  : Unsigned_16;
      Month : Unsigned_8;
      Day   : Unsigned_8;
   end record;

   type TimeOnly is record
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
      Second : Unsigned_8;
   end record;

   type DateTime is record
      Year   : Unsigned_16;
      Month  : Unsigned_8;
      Day    : Unsigned_8;
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
      Second : Unsigned_8;
   end record;

   type HHmm is record
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
   end record;

   type Controller_Record is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : Hardware_Addr;
      Firmware : Unbounded_String;
      Date     : DateOnly;
   end record;

   type Controller_Record_List is array (Positive range <>) of Controller_Record;

   type Listener_Record is record
      Listener : GNAT.Sockets.Sock_Addr_Type;
      Interval : Unsigned_8;
   end record;

   type Door_State is record
      Open     : Boolean;
      Button   : Boolean;
      Unlocked : Boolean;
   end record;

   type Doors_State is array (1 .. 4) of Door_State;

   type Alarms_Record is record
      Flags       : Unsigned_8;
      Fire        : Boolean;
      Lock_Forced : Boolean;
   end record;

   type Controller_State is record
      System_Date_Time : DateTime;
      Doors            : Doors_State;
      Alarms           : Alarms_Record;
      System_Error     : Unsigned_8;
      Special_Info     : Unsigned_8;
   end record;

   type Event_Type is (Unknown, Swipe, Door, Alarm, Overwritten);

   for Event_Type use (
      Unknown     => 0,
      Swipe       => 1,
      Door        => 2,
      Alarm       => 3,
      Overwritten => 255);

   function To_Event_Type (V : Unsigned_8) return Event_Type;

   type Event_Direction is (Direction_Unknown, Direction_In, Direction_Out);

   for Event_Direction use (
      Direction_Unknown => 0,
      Direction_In      => 1,
      Direction_Out     => 2);

   function To_Event_Direction (V : Unsigned_8) return Event_Direction;

   type Event_Reason is (
      SwipeOk,
      Denied_PC_Control,
      Denied_Not_Allowed,
      Denied_Incorrect_PIN,
      Denied_Anti_Passback,
      Denied_More_Cards,
      Denied_First_Card_Required,
      Denied_Door_Normally_Closed,
      Denied_Door_InterLock,
      Denied_Time_Profile,
      Denied_Invalid_Timezone,
      Denied_Invalid,
      Push_Button_Ok,
      Door_Open,
      Door_Closed,
      Supervisor_Override,
      Controller_Power_On,
      Controller_Reset,
      Push_Button_Disabled,
      Push_Button_Lock_Forced,
      Push_Button_Offline,
      Push_Button_Door_InterLock,
      Threat,
      Open_Too_Long,
      Forced_Open,
      Fire,
      Forced_Close,
      Theft_Prevention,
      Zone_24x7,
      Emergency_Call,
      Remote_Open_Door,
      Remote_Open_Door_USB,
      Other);

   for Event_Reason use (SwipeOk                     => 16#01#,   --  1
                         Denied_PC_Control           => 16#05#,   --  5
                         Denied_Not_Allowed          => 16#06#,   --  6
                         Denied_Incorrect_PIN        => 16#07#,   --  7
                         Denied_Anti_Passback        => 16#08#,   --  8
                         Denied_More_Cards           => 16#09#,   --  9
                         Denied_First_Card_Required  => 16#0A#,   --  10
                         Denied_Door_Normally_Closed => 16#0B#,   --  11
                         Denied_Door_InterLock       => 16#0C#,   --  12
                         Denied_Time_Profile         => 16#0D#,   --  13
                         Denied_Invalid_Timezone     => 16#0F#,   --  15
                         Denied_Invalid              => 16#12#,   --  18
                         Push_Button_Ok              => 16#14#,   --  20
                         Door_Open                   => 16#17#,   --  23
                         Door_Closed                 => 16#18#,   --  24
                         Supervisor_Override         => 16#19#,   --  25
                         Controller_Power_On         => 16#1C#,   --  28
                         Controller_Reset            => 16#1D#,   --  29
                         Push_Button_Disabled        => 16#1E#,   --  30
                         Push_Button_Lock_Forced     => 16#1F#,   --  31
                         Push_Button_Offline         => 16#20#,   --  32
                         Push_Button_Door_InterLock  => 16#21#,   --  33
                         Threat                      => 16#22#,   --  34
                         Open_Too_Long               => 16#25#,   --  37
                         Forced_Open                 => 16#26#,   --  38
                         Fire                        => 16#27#,   --  39
                         Forced_Close                => 16#28#,   --  40
                         Theft_Prevention            => 16#29#,   --  41
                         Zone_24x7                   => 16#2A#,   --  42
                         Emergency_Call              => 16#2B#,   --  43
                         Remote_Open_Door            => 16#2C#,   --  44
                         Remote_Open_Door_USB        => 16#2D#,   --  45
                         Other                       => 16#FF#);  --  255

   function To_Event_Reason (V : Unsigned_8) return Event_Reason;

   type Controller_Event is record
      Index          : Unsigned_32;
      Event          : Event_Type;
      Timestamp      : DateTime;
      Door           : Unsigned_8;
      Direction      : Event_Direction;
      Card           : Unsigned_32;
      Access_Granted : Boolean;
      Reason         : Event_Reason;
   end record;

   type Controller_Status is record
      State : Controller_State;
      Event : Controller_Event;
   end record;

   type Control_Mode is (Normally_Open, Normally_Closed, Controlled, First_Card_Only);

   for Control_Mode use (Normally_Open   => 1,
                         Normally_Closed => 2,
                         Controlled      => 3,
                         First_Card_Only => 4);

   type Door_Record is record
      Mode      : Control_Mode;
      OpenDelay : Unsigned_8;
   end record;

   type Card_Record is record
      Card       : Unsigned_32;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_24;
   end record;

   function To_Control_Mode (V : Unsigned_8) return Control_Mode;

   type Passcodes_List is array (Positive range <>) of Unsigned_32;

   type Weekdays_Type is record
      Monday          : Boolean;
      Tuesday         : Boolean;
      Wednesday       : Boolean;
      Thursday        : Boolean;
      Friday          : Boolean;
      Saturday        : Boolean;
      Sunday          : Boolean;
   end record;

   type Segment is record
      Start_Time : HHmm;
      End_Time   : HHmm;
   end record;

   type Segments_List is array (1 .. 3) of Segment;

   type Time_Profile is record
      Start_Date     : DateOnly;
      End_Date       : DateOnly;
      Weekdays       : Weekdays_Type;
      Segments       : Segments_List;
      Linked_Profile : Unsigned_8;
   end record;

   type Task_Type is (Door_Controlled,
                      Door_Normally_Open,
                      Door_Normally_Closed,
                      Disable_Time_Profile,
                      Enable_Time_Profile,
                      Card_No_Password,
                      Card_In_Password,
                      Card_InOut_Password,
                      Enable_More_Cards,
                      Disable_More_Cards,
                      Trigger_Once,
                      Disable_PushButton,
                      Enable_PushButton);

   for Task_Type use (Door_Controlled      => 0,
                      Door_Normally_Open   => 1,
                      Door_Normally_Closed => 2,
                      Disable_Time_Profile => 3,
                      Enable_Time_Profile  => 4,
                      Card_No_Password     => 5,
                      Card_In_Password     => 6,
                      Card_InOut_Password  => 7,
                      Enable_More_Cards    => 8,
                      Disable_More_Cards   => 9,
                      Trigger_Once         => 10,
                      Disable_PushButton   => 11,
                      Enable_PushButton    => 12);

   function To_Task_Type (V : Unsigned_8) return Task_Type;

   type Task_Record is record
      Task_ID        : Task_Type;
      Start_Date     : DateOnly;
      End_Date       : DateOnly;
      Weekdays       : Weekdays_Type;
      Start_Time     : HHmm;
      Door           : Unsigned_8;
      More_Cards     : Unsigned_8;
   end record;

   type Interlock is (No_Interlock, Interlock_12, Interlock_34, Interlock_12_34, Interlock_123, Interlock_1234);

   for Interlock use (No_Interlock    => 16#00#,
                      Interlock_12    => 16#01#,    --  doors (1,2)
                      Interlock_34    => 16#02#,    --  doors (3,4)
                      Interlock_12_34 => 16#03#,    --  doors (1,2) and (3,4)
                      Interlock_123   => 16#04#,    --  doors (1,2,3)
                      Interlock_1234  => 16#08#);   --  doors (1,2,3,4)

   function To_Interlock (V : Unsigned_8) return Interlock;

   type Keypads is array (1 .. 4) of Boolean;

   type Antipassback is (No_Antipassback, Readers_12_34, Readers_13_24, Readers_1_23, Readers_1_234);

   for Antipassback use (No_Antipassback => 16#00#,   --  disabled
                         Readers_12_34   => 16#01#,   --  readers 1:2; 3:4 (independently)
                         Readers_13_24   => 16#02#,   --  readers (1,3):(2,4)
                         Readers_1_23    => 16#03#,   --  readers 1:(2,3)
                         Readers_1_234   => 16#04#);  --  readers 1:(2,3,4)

   function To_Antipassback (V : Unsigned_8) return Antipassback;

   type First_Card_Record is record
      Start_Time    : HHmm;
      End_Time      : HHmm;
      Active_Mode   : Control_Mode;
      Inactive_Mode : Control_Mode;
      Weekdays      : Weekdays_Type;
   end record;

   type Signal is new Ada.Finalization.Limited_Controlled with record
      Selector : GNAT.Sockets.Selector_Type;
   end record;

   overriding procedure Initialize (S : in out Signal);
   overriding procedure Finalize   (S : in out Signal);
   procedure Trigger (S : in out Signal);

   function Image (Addr : IPv4) return String;
   function Image (MAC  : Hardware_Addr) return String;
   function Image (D    : DateOnly) return String;
   function Image (T    : TimeOnly) return String;
   function Image (DT   : DateTime) return String;
   function Image (V    : Event_Direction) return String;
   function Image (T    : HHmm) return String;

end Uhppoted.Types;
