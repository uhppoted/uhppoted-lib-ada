with Interfaces;
with Ada.Strings.Unbounded;
with GNAT.Sockets;

package Uhppoted.Types is
   use Ada.Strings.Unbounded;
   use Interfaces;

   --  Custom exception for invalid destination address errors.
   Invalid_Address_Error : exception;

   --  Custom exception for card not found errors.
   Card_Not_Found_Error : exception;

   --  Custom exception for card deleted errors.
   Card_Deleted_Error : exception;

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

   type Door_Type is record
      Open     : Boolean;
      Button   : Boolean;
      Unlocked : Boolean;
   end record;

   type Doors_Type is array (1 .. 4) of Door_Type;

   type Alarms_Type is record
      Flags       : Unsigned_8;
      Fire        : Boolean;
      Lock_Forced : Boolean;
   end record;

   type Event_Type is record
      Index          : Unsigned_32;
      Event          : Unsigned_8;
      Timestamp      : DateTime;
      Door           : Unsigned_8;
      Direction      : Unsigned_8;
      Card           : Unsigned_32;
      Access_Granted : Boolean;
      Reason         : Unsigned_8;
   end record;

   type Controller_Status is record
      System_Date_Time : DateTime;
      Doors            : Doors_Type;
      Alarms           : Alarms_Type;
      System_Error     : Unsigned_8;
      Special_Info     : Unsigned_8;
      Event            : Event_Type;
   end record;

   type Control_Mode is (Normally_Open, Normally_Closed, Controlled);

   for Control_Mode use (
      Normally_Open   => 1,
      Normally_Closed => 2,
      Controlled      => 3);

   type Door_Record is record
      Mode      : Control_Mode;
      OpenDelay : Unsigned_8;
   end record;

   type Card_Type is record
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

   function Image (Addr : IPv4) return String;
   function Image (MAC : Hardware_Addr) return String;
   function Image (D : DateOnly) return String;
   function Image (T : TimeOnly) return String;
   function Image (DT : DateTime) return String;

end Uhppoted.Types;
