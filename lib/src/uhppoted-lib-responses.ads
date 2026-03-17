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
      Controller  : Unsigned_32;
      Date_Time   : DateTime;
   end record;

   --  Message definition for a set-time response.
   type Set_Time_Response is record
      Controller  : Unsigned_32;
      Date_Time   : DateTime;
   end record;

   --  Message definition for a get-listener response.
   type Get_Listener_Response is record
      Controller : Unsigned_32;
      Address    : IPv4;
      Port       : Unsigned_16;
      Interval   : Unsigned_8;
   end record;

   --  Message definition for a set-listener response.
   type Set_Listener_Response is record
      Controller : Unsigned_32;
      OK         : Boolean;
   end record;

   --  Message definition for a get-status response.
   type Relay_State  is mod 256;
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

end Uhppoted.Lib.Responses;
