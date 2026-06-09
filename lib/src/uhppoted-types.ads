with Interfaces;
with Ada.Finalization;
with Ada.Strings.Unbounded;
with GNAT.Sockets;

--  Public types for use with the API library for the UHPPOTE access controllers.
--

package Uhppoted.Types is
   use Ada.Strings.Unbounded;
   use Interfaces;

   --  Custom error raised when the bind, broadcast or listen address is invalid.
   Invalid_Address_Error : exception;

   --  Custom error raised when the door control mode is not valid.
   Invalid_Door_Mode_Error : exception;

   --  Custom error raised when there is no card record at an index.
   Card_Not_Found_Error : exception;

   --  Custom error raised when the card record at an index has been deleted.
   Card_Deleted_Error : exception;

   --  Custom error raised when there is no event record at an index.
   Event_Not_Found_Error : exception;

   --  Custom error raised when the event record at an index has been overwritten.
   Event_Overwritten_Error : exception;

   --  Custom error raised when there is no time profile record at an ID.
   Time_Profile_Not_Found_Error : exception;

   --  Custom error raised when the response from a controller does not match the controller ID, is the incorrect
   --  response type or has the incorrect information.
   Invalid_Response_Error : exception;

   --  Custom error raised when a controller does respond within the time limit.
   Timeout_Error : exception;

   --  IPv4 address byte array.
   type IPv4 is array (1 .. 4) of Interfaces.Unsigned_8;

   --  MAC address byte array.
   type Hardware_Addr is array (1 .. 6) of Unsigned_8;

   --  Firmware version string.
   subtype Firmware_Version is String (1 .. 5);

   --  Utility type for a date.
   --
   --  @field  Year    4 digit year.
   --  @field  Month   Month of year [1..12].
   --  @field  Day     Day of month [1..31].
   type DateOnly is record
      Year  : Unsigned_16;
      Month : Unsigned_8;
      Day   : Unsigned_8;
   end record;

   --  Utility type for a time of day with a resolution of seconds.
   --
   --  @field  Hour    Hour of day [0..23].
   --  @field  Minute  Minute of hour [0..59].
   --  @field  Second  Second of minute [0..59].
   type TimeOnly is record
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
      Second : Unsigned_8;
   end record;

   --  Utility type for a date/time without timezone information.
   --
   --  @field  Year    4 digit year.
   --  @field  Month   Month of year [1..12].
   --  @field  Day     Day of month [1..31].
   --  @field  Hour    Hour of day [0..23].
   --  @field  Minute  Minute of hour [0..59].
   --  @field  Second  Second of minute [0..59].
   type DateTime is record
      Year   : Unsigned_16;
      Month  : Unsigned_8;
      Day    : Unsigned_8;
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
      Second : Unsigned_8;
   end record;

   --  Utility type for a time of day with a resolution of minutes.
   --
   --  @field  Hour    Hour of day [0..23].
   --  @field  Minute  Minute of hour [0..59].
   type HHmm is record
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
   end record;

   --  Container record for the controller static information.
   --
   --  @field  ID       Controller serial number.
   --  @field  Address  IPv4 controller address.
   --  @field  Netmask  IPv4 subnet mask.
   --  @field  Gateway  IPv4 gateway address.
   --  @field  MAC      Controller MAC address (hex format).
   --  @field  Firmware Controller firmware version (e.g. v6.62).
   --  @field  Date     Controller firmware release date.
   type Controller_Record is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : Hardware_Addr;
      Firmware : Unbounded_String;
      Date     : DateOnly;
   end record;

   --  List of controllers returned by Find_Controllers.
   type Controller_Record_List is array (Positive range <>) of Controller_Record;

   --  Container record for the controller event listener configuration.
   --
   --  @field  Listener  IPv4 address:port to which events are sent.
   --  @field  Interval  Interval (seconds) at which to send the current controller state to
   --                    the configured listener.
   type Listener_Record is record
      Listener : GNAT.Sockets.Sock_Addr_Type;
      Interval : Unsigned_8;
   end record;

   --  Container record for a door state.
   --
   --  @field  Open      True if the door open sensor is set.
   --  @field  Button    True if the door pushbutton is pressed.
   --  @field  Unlocked  True if the door unlock relay is active.
   type Door_State is record
      Open     : Boolean;
      Button   : Boolean;
      Unlocked : Boolean;
   end record;

   --   List of controller doors with associated state.
   type Doors_State is array (1 .. 4) of Door_State;

   --  Container record for a controller alarms.
   --
   --  @field  Flags        Bitmask of controller alarms.
   --  @field  Fire         Fire alarm input state.
   --  @field  Lock_Forced  Lock forced alarm input state.
   type Alarms_Record is record
      Flags       : Unsigned_8;
      Fire        : Boolean;
      Lock_Forced : Boolean;
   end record;

   --  Container record for a controller state.
   --
   --  @field  System_Date_Time  Controller current date/time.
   --  @field  Doors             List of doors with corresponding door state.
   --  @field  Alarms            List of alarms with corresponding alarm state.
   --  @field  System_Error      System error code (0 for none).
   --  @field  Special_Info      Whatever that may be.
   type Controller_State is record
      System_Date_Time : DateTime;
      Doors            : Doors_State;
      Alarms           : Alarms_Record;
      System_Error     : Unsigned_8;
      Special_Info     : Unsigned_8;
   end record;

   --  Event type enum.
   --
   --  @enum Unknown      Unknown event.
   --  @enum Swipe        Card swipe event.
   --  @enum Door         Door event.
   --  @enum Alarm        Alarm event.
   --  @enum Overwritten  Overwritten event.
   type Event_Type is (Unknown, Swipe, Door, Alarm, Overwritten);

   for Event_Type use (Unknown => 0, Swipe => 1, Door => 2, Alarm => 3, Overwritten => 255);

   --  Utility function to translate an Unsigned_8 to an event type enum.
   --
   --  0:   Unknown
   --  1:   Swipe
   --  2:   Door
   --  3:   Alarm
   --  255: Overwritten
   --
   --  @param      V            Unsigned_8 value to translate.  Unknown values are translated to Unknown.
   --  @return     Event_Type   Translated event type.
   function To_Event_Type (V : Unsigned_8) return Event_Type;

   --  Event direction enum.
   --
   --  @enum Direction_Unknown  Unknown.
   --  @enum Direction_In       In.
   --  @enum Direction_Out      Out.
   type Event_Direction is (Direction_Unknown, Direction_In, Direction_Out);

   for Event_Direction use (Direction_Unknown => 0, Direction_In => 1, Direction_Out => 2);

   --  Utility function to translate an Unsigned_8 to an event direction enum.
   --
   --  0:   Direction_Unknown
   --  1:   Direction_In
   --  2:   Direction_Out
   --
   --  @param      V                 Unsigned_8 value to translate.  Unknown values are translated to Direction_Unknown.
   --  @return     Event_Direction   Translated event direction.
   function To_Event_Direction (V : Unsigned_8) return Event_Direction;

   --  Event reason enum.
   --
   --  @enum SwipeOk                      Access granted (valid card swipe).
   --  @enum Denied_PC_Control            Access denied (remote access control).
   --  @enum Denied_Not_Allowed           Access denied (card does not have access privileges).
   --  @enum Denied_Incorrect_PIN         Access denied (invalid PIN).
   --  @enum Denied_Anti_Passback         Access denied (anti-passback).
   --  @enum Denied_More_Cards            Access denied (more cards not enabled/limited).
   --  @enum Denied_First_Card_Required   Access denied (door requires first card activation).
   --  @enum Denied_Door_Normally_Closed  Access denied (door is locked).
   --  @enum Denied_Door_InterLock        Access denied (interlocked door is open).
   --  @enum Denied_Time_Profile          Access denied (denied by time profile).
   --  @enum Denied_Invalid_Timezone      Access denied (denied by time profile segment).
   --  @enum Denied_Invalid               Access denied (invalid card).
   --  @enum Push_Button_Ok               Access granted (push button).
   --  @enum Door_Open                    Door open event.
   --  @enum Door_Closed                  Door close event.
   --  @enum Supervisor_Override          Supervisor passcode used to open door.
   --  @enum Controller_Power_On          Controller power on.
   --  @enum Controller_Reset             Controller reset.
   --  @enum Push_Button_Disabled         Pushbutton access denied (disabled).
   --  @enum Push_Button_Lock_Forced      Pushbutton access denied (lock forced).
   --  @enum Push_Button_Offline          Pushbutton access denied (offline).
   --  @enum Push_Button_Door_InterLock   Pushbutton access denied (door interlock).
   --  @enum Threat                       Warning: threat.
   --  @enum Open_Too_Long                Warning: door held open.
   --  @enum Forced_Open                  Warning: door opened without unlock.
   --  @enum Fire                         Warning: fire.
   --  @enum Forced_Close                 Warning: door closed.
   --  @enum Theft_Prevention             Warning: tamper detect input.
   --  @enum Zone_24x7                    Warning: 24x7 zone.
   --  @enum Emergency_Call               Warning: emergency.
   --  @enum Remote_Open_Door             Warning: door opened remotely.
   --  @enum Remote_Open_Door_USB         Warning: door opened by USB reader.
   --  @enum Other                        Unknown reason.
   type Event_Reason is
     (SwipeOk,
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

   for Event_Reason use
     (SwipeOk                     => 16#01#,
      Denied_PC_Control           => 16#05#,
      Denied_Not_Allowed          => 16#06#,
      Denied_Incorrect_PIN        => 16#07#,
      Denied_Anti_Passback        => 16#08#,
      Denied_More_Cards           => 16#09#,
      Denied_First_Card_Required  => 16#0A#,
      Denied_Door_Normally_Closed => 16#0B#,
      Denied_Door_InterLock       => 16#0C#,
      Denied_Time_Profile         => 16#0D#,
      Denied_Invalid_Timezone     => 16#0F#,
      Denied_Invalid              => 16#12#,
      Push_Button_Ok              => 16#14#,
      Door_Open                   => 16#17#,
      Door_Closed                 => 16#18#,
      Supervisor_Override         => 16#19#,
      Controller_Power_On         => 16#1C#,
      Controller_Reset            => 16#1D#,
      Push_Button_Disabled        => 16#1E#,
      Push_Button_Lock_Forced     => 16#1F#,
      Push_Button_Offline         => 16#20#,
      Push_Button_Door_InterLock  => 16#21#,
      Threat                      => 16#22#,
      Open_Too_Long               => 16#25#,
      Forced_Open                 => 16#26#,
      Fire                        => 16#27#,
      Forced_Close                => 16#28#,
      Theft_Prevention            => 16#29#,
      Zone_24x7                   => 16#2A#,
      Emergency_Call              => 16#2B#,
      Remote_Open_Door            => 16#2C#,
      Remote_Open_Door_USB        => 16#2D#,
      Other                       => 16#FF#);

   --  Utility function to translate an Unsigned_8 to an event reason enum.
   --
   --  1:   SwipeOk
   --  5:   Denied_PC_Control
   --  6:   Denied_Not_Allowed
   --  7:   Denied_Incorrect_PIN
   --  8:   Denied_Anti_Passback
   --  9:   Denied_More_Cards
   --  10:  Denied_First_Card_Required
   --  11:  Denied_Door_Normally_Closed
   --  12:  Denied_Door_InterLock
   --  13:  Denied_Time_Profile
   --  15:  Denied_Invalid_Timezone
   --  18:  Denied_Invalid
   --  20:  Push_Button_Ok
   --  23:  Door_Open
   --  24:  Door_Closed
   --  25:  Supervisor_Override
   --  28:  Controller_Power_On
   --  29:  Controller_Reset
   --  30:  Push_Button_Disabled
   --  31:  Push_Button_Lock_Forced
   --  32:  Push_Button_Offline
   --  33:  Push_Button_Door_InterLock
   --  34:  Threat
   --  37:  Open_Too_Long
   --  38:  Forced_Open
   --  39:  Fire
   --  40:  Forced_Close
   --  41:  Theft_Prevention
   --  42:  Zone_24x7
   --  43:  Emergency_Call
   --  44:  Remote_Open_Door
   --  45:  Remote_Open_Door_USB
   --  255: Other
   --
   --  @param  V             Unsigned_8 value to translate. Unknown values are translated to Other.
   --  @return Event_Reason  Translated event reason.
   function To_Event_Reason (V : Unsigned_8) return Event_Reason;

   --  Container record for a controller event.
   --
   --  @field  Index           Event record index.
   --  @field  Event           Event type.
   --  @field  Timestamp       Date/time at which event occurred.
   --  @field  Door            Door ID for access events.
   --  @field  Direction       Direction (IN/OUT) for access events.
   --  @field  Card            Card number for access events.
   --  @field  Access_Granted  True if access was granted.
   --  @field  Reason          Event reason code.
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

   --  Container record for the current state of a controller.
   --
   --  @field  State  Current controller state.
   --  @field  Event  Most recent event (if any).
   type Controller_Status is record
      State : Controller_State;
      Event : Controller_Event;
   end record;

   --  Door control mode enum.
   --
   --  @enum Normally_Open    Door is permanently unlocked.
   --  @enum Normally_Closed  Door is permanently locked.
   --  @enum Controlled       Access requires a valid card swipe.
   --  @enum First_Card_Only  Access requires a valid 'first card' swipe.
   type Control_Mode is (Normally_Open, Normally_Closed, Controlled, First_Card_Only);

   for Control_Mode use (Normally_Open => 1, Normally_Closed => 2, Controlled => 3, First_Card_Only => 4);

   --  Container record for a door control configuration.
   --
   --  @field  Mode        Door control mode (controlled, normally open or normally closed).
   --  @field  Open_Delay  Time in seconds for which door will remain unlocked.
   type Door_Record is record
      Mode       : Control_Mode;
      Open_Delay : Unsigned_8;
   end record;

   --  Container record for an access card.
   --
   --  @field  Card        Wiegand-26 card number.
   --  @field  Start_Date  Date from which card is valid.
   --  @field  End_Date    Date after which card is no longer valid.
   --  @field  Door_1      Access privileges for door 1 (0: none, 1: 24x7, 2..254: time profile).
   --  @field  Door_2      Access privileges for door 2 (0: none, 1: 24x7, 2..254: time profile).
   --  @field  Door_3      Access privileges for door 3 (0: none, 1: 24x7, 2..254: time profile).
   --  @field  Door_4      Access privileges for door 4 (0: none, 1: 24x7, 2..254: time profile).
   --  @field  PIN         Access reader PIN code (0 for none).
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

   --  Utility function to translate an Unsigned_8 to a door Control_Mode enum.
   --
   --  - 1:  Normally_Open
   --  - 2:  Normally_Closed
   --  - 3:  Controlled
   --  - 4:  First_Card_Only
   --
   --  @param      V                 Unsigned_8 value to translate.
   --  @return     Control_Mode      Translated door control mode enum.
   --  @exception  Constraint_Error  Raised if V is not a valid value.
   function To_Control_Mode (V : Unsigned_8) return Control_Mode;

   --  Container for a list of supervisor override codes. Valid codes are in the range [0..999999].
   type Passcodes_List is array (Positive range <>) of Unsigned_32;

   --  Container record for the days of week on which a Time_Profile, Task or FirstCard is enabled.
   --
   --  @field  Monday     True if enabled on Monday.
   --  @field  Tuesday    True if enabled on Tuesday.
   --  @field  Wednesday  True if enabled on Wednesday.
   --  @field  Thursday   True if enabled on Thursday.
   --  @field  Friday     True if enabled on Friday.
   --  @field  Saturday   True if enabled on Saturday.
   --  @field  Sunday     True if enabled on Sunday.
   type Weekdays_Type is record
      Monday    : Boolean;
      Tuesday   : Boolean;
      Wednesday : Boolean;
      Thursday  : Boolean;
      Friday    : Boolean;
      Saturday  : Boolean;
      Sunday    : Boolean;
   end record;

   --  Container record for a single Time_Profile time segment.
   --
   --  @field  Start_Time  Time of day from from which time profile is active.
   --  @field  End_Time    Time of day after which time profile is no longer active.
   type Time_Segment is record
      Start_Time : HHmm;
      End_Time   : HHmm;
   end record;

   --  Container for the segments in a Time_Profile.
   type Segments_List is array (1 .. 3) of Time_Segment;

   --  Container record for the information for a time profile.
   --
   --  @field  Start_Date      Date from which time profile is active.
   --  @field  End_Date        Date after which time profile is no longer active.
   --  @field  Weekdays        Days of week on which time profile is active.
   --  @field  Segments        List of time segments during which time profile is active.
   --  @field  Linked_Profile  Time profile ID with additional constraints/segments for the same door (0 for none).
   type Time_Profile is record
      Start_Date     : DateOnly;
      End_Date       : DateOnly;
      Weekdays       : Weekdays_Type;
      Segments       : Segments_List;
      Linked_Profile : Unsigned_8;
   end record;

   --  Task type enum.
   --
   --  @enum Door_Controlled       Sets door control mode to controlled.
   --  @enum Door_Normally_Open    Sets door control mode to normally open.
   --  @enum Door_Normally_Closed  Sets door control mode to normally closed.
   --  @enum Disable_Time_Profile  Disables any time profiles assigned to the door.
   --  @enum Enable_Time_Profile   Enables any time profiles assigned to the door.
   --  @enum Card_No_Password      Enables card swipe without a PIN.
   --  @enum Card_In_Password      Enables PINs on an IN card swipe.
   --  @enum Card_InOut_Password   Enables PINS on both IN and OUT card swipes.
   --  @enum Enable_More_Cards     Enables 'more cards'.
   --  @enum Disable_More_Cards    Disables 'more cards'.
   --  @enum Trigger_Once          Unlocks the door.
   --  @enum Disable_PushButton    Disables the door pushbutton.
   --  @enum Enable_PushButton     Enables the door pushbutton.
   type Task_Type is
     (Door_Controlled,
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

   for Task_Type use
     (Door_Controlled      => 0,
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

   --  Utility function to translate an Unsigned_8 to a Task type enum.
   --
   --  - 0:  Door_Controlled
   --  - 1:  Door_Normally_Open
   --  - 2:  Door_Normally_Closed
   --  - 3:  Disable_Time_Profile
   --  - 4:  Enable_Time_Profile
   --  - 5:  Card_No_Password
   --  - 6:  Card_In_Password
   --  - 7:  Card_InOut_Password
   --  - 8:  Enable_More_Cards
   --  - 9:  Disable_More_Cards
   --  - 10: Trigger_Once
   --  - 11: Disable_PushButton
   --  - 12: Enable_PushButton
   --
   --  @param  V          Unsigned_8 value to translate.
   --  @return Task_Type  Translated task type enum.
   --  @exception  Constraint_Error  Raised if V is not a valid value.
   function To_Task_Type (V : Unsigned_8) return Task_Type;

   --  Container record for the information for a scheduled task.
   --
   --  @field  Task_ID     Task to execute at the scheduled time.
   --  @field  Start_Date  Date from which task will be scheduled.
   --  @field  End_Date    Date after which task will no longer be scheduled.
   --  @field  Weekdays    Days of week on which task will be scheduled.
   --  @field  Start_Time  Time of day from which the 'first card 'mode can be activated by a card swipe.
   --  @field  Door        Door ID [1..4] for door affected by task.
   --  @field  More_Cards  Number of 'more cards' for the 'More Cards' task.
   type Task_Record is record
      Task_ID    : Task_Type;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Weekdays   : Weekdays_Type;
      Start_Time : HHmm;
      Door       : Unsigned_8;
      More_Cards : Unsigned_8;
   end record;

   --  Interlock enum.
   --
   --  @enum No_Interlock     Disables door interlocks.
   --  @enum Interlock_12     Interlocks doors 1 and 2.
   --  @enum Interlock_34     Interlocks doors 3 and 4.
   --  @enum Interlock_12_34  Interlocks doors 1 and 2 and interlocks doors 3 and 4 (independently).
   --  @enum Interlock_123    Interlocks doors 1, 2 and 3.
   --  @enum Interlock_1234   Interlocks doors 1, 2, 3 and 4.
   type Interlock is (No_Interlock, Interlock_12, Interlock_34, Interlock_12_34, Interlock_123, Interlock_1234);

   for Interlock use
     (No_Interlock    => 16#00#,
      Interlock_12    => 16#01#,
      Interlock_34    => 16#02#,
      Interlock_12_34 => 16#03#,
      Interlock_123   => 16#04#,
      Interlock_1234  => 16#08#);

   --  Utility function to translate an Unsigned_8 to an Interlock enum.
   --
   --  - 0: No_Interlock
   --  - 1: Interlock_12
   --  - 2: Interlock_34
   --  - 3: Interlock_12_34
   --  - 4: Interlock_123
   --  - 8: Interlock_123
   --
   --  @param  V          Unsigned_8 value to translate.
   --  @return Interlock  Translated interlock enum.
   --  @exception         Constraint_Error  Raised if V is not a valid value.
   function To_Interlock (V : Unsigned_8) return Interlock;

   --  Range type for reader access keypads (1..4]).
   type Keypads is array (1 .. 4) of Boolean;

   --  Anti-passback enum.
   --
   --  @enum No_Antipassback  Disables anti-passback.
   --  @enum Readers_12_34    Configures anti-passback between reader 1 and reader 2 and also between
   --                         reader 3 and reader 4 (independently).
   --  @enum Readers_13_24    Configures anti-passback between readers 1 or 3 and readers 2 or 4.
   --  @enum Readers_1_23     Configures anti-passback between reader 1 and readers 2 or 3.
   --  @enum Readers_1_234    Configures anti-passback between reader 1 and readers 2, 3 or 4.
   type Antipassback is (No_Antipassback, Readers_12_34, Readers_13_24, Readers_1_23, Readers_1_234);

   for Antipassback use
     (No_Antipassback => 16#00#,
      Readers_12_34   => 16#01#,
      Readers_13_24   => 16#02#,
      Readers_1_23    => 16#03#,
      Readers_1_234   => 16#04#);

   --  Utility function to translate an Unsigned_8 to an Antipassback enum.
   --
   --  - 0: No_Antipassback
   --  - 1: Readers_12_34
   --  - 2: Readers_13_24
   --  - 3: Readers_1_23
   --  - 4: Readers_1_234
   --
   --  @param  V             Unsigned_8 value to translate.
   --  @return Antipassback  Translated anti-passback enum.
   --  @exception  Constraint_Error  Raised if V is not a valid value.
   function To_Antipassback (V : Unsigned_8) return Antipassback;

   --  Container record for the configuration information for controller managed door first-card mode.
   --
   --  @field  Start_Time     Time of day from which the 'first card 'mode can be activated by a card swipe.
   --  @field  End_Time       Time of day after which 'first card' mode is no activated.
   --  @field  Active_Mode    Door control mode after a valid 'first card' swipe. Valid values are Controlled,
   --                         Normally_Open or Normally_Closed.
   --  @field  Inactive_Mode  Door control mode when 'first card' mode is no active. Valid values are Controlled,
   --                         Normally_Open, Normally_Closed or First_Card_Only.
   --  @field  Weekdays       Days of week on which 'first card' mode can be activated.
   type First_Card_Record is record
      Start_Time    : HHmm;
      End_Time      : HHmm;
      Active_Mode   : Control_Mode;
      Inactive_Mode : Control_Mode;
      Weekdays      : Weekdays_Type;
   end record;

   --  Signal type for asynchronously terminating a Listen operation. Implements Limited_Controlled.
   --
   --  @field  Selector  Internal selector for 'select' operation.
   type Signal is new Ada.Finalization.Limited_Controlled with record
      Selector : GNAT.Sockets.Selector_Type;
   end record;

   --  Controlled.Initialize implementation for a Signal.
   --
   --  @param  S  Signal to initialise.
   overriding
   procedure Initialize (S : in out Signal);

   --  Controlled.Finalize implementation for a Signal.
   --
   --  @param  S  Signal to cleanup.
   overriding
   procedure Finalize (S : in out Signal);

   --  Utility procedure to raise a signal.
   --
   --  @param  S  Signal to raise.
   procedure Trigger (S : in out Signal);

   --  Returns the IPv4 address formatted as "n.n.n.n", e.g. "192.168.1.100".
   --
   --  @param  Addr  IPv4 address to format.
   --  @return       IPv4 address in dotted decimal format.
   function Image (Addr : IPv4) return String;

   --  Returns the MAC address formatted as "xx:xx:xx:xx:xx:xx".
   --
   --  @param  MAC  MAC address to format.
   --  @return      MAC address in hex format.
   function Image (MAC : Hardware_Addr) return String;

   --  Returns the date value formatted as "yyyy-mm-dd" e.g. "2026-06-23".
   --
   --  @param  D  The date value to format.
   --  @return    Date formatted as "yyyy-mm-dd".
   function Image (D : DateOnly) return String;

   --  Returns the time value formatted as "HH:mm:ss" e.g. "13:45:56".
   --
   --  @param  T  The time value to format.
   --  @return    Time formatted as "HH:mm:ss".
   function Image (T : TimeOnly) return String;

   --  Returns the date/time value formatted as "yyyy-mm-dd HH:mm:ss" e.g. "2026-06-23 13:45:56".
   --
   --  @param  DT  The date/time value to format.
   --  @return     Date/time formatted as "yyyy-mm-dd HH:mm:ss".
   function Image (DT : DateTime) return String;

   --  Returns the event direction formatted as "IN" or "OUT".
   --
   --  @param  V Event direction to format.
   --  @return   "IN" or "OUT".
   function Image (V : Event_Direction) return String;

   --  Returns the HHmm value formatted as "HH:mm" e.g. "13:45".
   --
   --  @param T The HHmm time value to format.
   --  @return A string representation of the time in "HH:mm" format (e.g., "13:45").
   function Image (T : HHmm) return String;

end Uhppoted.Types;
