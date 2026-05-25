with Interfaces;
with GNAT.Sockets;
with Uhppoted.Types;

--  API library for the UHPPOTE access controllers.
package Uhppoted.Lib is
   use Interfaces;
   use GNAT.Sockets;
   use Uhppoted.Types;

   type UHPPOTE is record
      Bind_Addr      : Sock_Addr_Type := (Addr => Any_Inet_Addr,       Family => Family_Inet, Port => 0);
      Broadcast_Addr : Sock_Addr_Type := (Addr => Broadcast_Inet_Addr, Family => Family_Inet, Port => 60000);
      Listen_Addr    : Sock_Addr_Type := (Addr => Any_Inet_Addr,       Family => Family_Inet, Port => 60001);
      Debug          : Boolean := False;
   end record;

   type Protocol_Type is (Default, UDP, TCP);

   type Controller is record
      ID       : Unsigned_32;
      DestAddr : Sock_Addr_Type := No_Sock_Addr;
      Protocol : Protocol_Type  := Default;
   end record;

   --  Interface type for an event handler.
   type Event_Handler is interface;

   procedure On_Event (Self       : Event_Handler;
                       Controller : Unsigned_32;
                       State      : Controller_State;
                       Event      : Controller_Event) is abstract;

   subtype Controller_Record      is Uhppoted.Types.Controller_Record;
   subtype Controller_Record_List is Uhppoted.Types.Controller_Record_List;
   subtype Controller_Status      is Uhppoted.Types.Controller_Status;
   subtype Controller_State       is Uhppoted.Types.Controller_State;
   subtype Controller_Event       is Uhppoted.Types.Controller_Event;
   subtype Door_Record            is Uhppoted.Types.Door_Record;
   subtype Card_Record            is Uhppoted.Types.Card_Record;
   subtype Listener_Record        is Uhppoted.Types.Listener_Record;
   subtype Time_Profile           is Uhppoted.Types.Time_Profile;
   subtype Time_Segment           is Uhppoted.Types.Segment;
   subtype Task_Record            is Uhppoted.Types.Task_Record;
   subtype Keypads                is Uhppoted.Types.Keypads;
   subtype Signal                 is Uhppoted.Types.Signal;

   procedure Trigger (S : in out Signal) renames Uhppoted.Types.Trigger;

   subtype DateTime       is Uhppoted.Types.DateTime;
   subtype HHmm           is Uhppoted.Types.HHmm;
   subtype Control_Mode   is Uhppoted.Types.Control_Mode;
   subtype Passcodes_List is Uhppoted.Types.Passcodes_List;
   subtype Task_Type      is Uhppoted.Types.Task_Type;
   subtype Interlock      is Uhppoted.Types.Interlock;
   subtype Antipassback   is Uhppoted.Types.Antipassback;

   Normally_Open        : Control_Mode renames Uhppoted.Types.Normally_Open;
   Normally_Closed      : Control_Mode renames Uhppoted.Types.Normally_Closed;
   Controlled           : Control_Mode renames Uhppoted.Types.Controlled;

   Door_Controlled      : Task_Type renames Uhppoted.Types.Door_Controlled;
   Door_Normally_Open   : Task_Type renames Uhppoted.Types.Door_Normally_Open;
   Door_Normally_Closed : Task_Type renames Uhppoted.Types.Door_Normally_Closed;
   Disable_Time_Profile : Task_Type renames Uhppoted.Types.Disable_Time_Profile;
   Enable_Time_Profile  : Task_Type renames Uhppoted.Types.Enable_Time_Profile;
   Card_No_Password     : Task_Type renames Uhppoted.Types.Card_No_Password;
   Card_In_Password     : Task_Type renames Uhppoted.Types.Card_In_Password;
   Card_InOut_Password  : Task_Type renames Uhppoted.Types.Card_InOut_Password;
   Enable_More_Cards    : Task_Type renames Uhppoted.Types.Enable_More_Cards;
   Disable_More_Cards   : Task_Type renames Uhppoted.Types.Disable_More_Cards;
   Trigger_Once         : Task_Type renames Uhppoted.Types.Trigger_Once;
   Disable_PushButton   : Task_Type renames Uhppoted.Types.Disable_PushButton;
   Enable_PushButton    : Task_Type renames Uhppoted.Types.Enable_PushButton;

   No_Interlock         : Interlock renames Uhppoted.Types.No_Interlock;
   Interlock_12         : Interlock renames Uhppoted.Types.Interlock_12;
   Interlock_34         : Interlock renames Uhppoted.Types.Interlock_34;
   Interlock_12_34      : Interlock renames Uhppoted.Types.Interlock_12_34;
   Interlock_123        : Interlock renames Uhppoted.Types.Interlock_123;
   Interlock_1234       : Interlock renames Uhppoted.Types.Interlock_1234;

   No_Antipassback      : Antipassback renames Uhppoted.Types.No_Antipassback;
   Readers_12_34        : Antipassback renames Uhppoted.Types.Readers_12_34;
   Readers_13_24        : Antipassback renames Uhppoted.Types.Readers_13_24;
   Readers_1_23         : Antipassback renames Uhppoted.Types.Readers_1_23;
   Readers_1_234        : Antipassback renames Uhppoted.Types.Readers_1_234;

   Invalid_Address_Error        : exception renames Uhppoted.Types.Invalid_Address_Error;
   Card_Not_Found_Error         : exception renames Uhppoted.Types.Card_Not_Found_Error;
   Card_Deleted_Error           : exception renames Uhppoted.Types.Card_Deleted_Error;
   Event_Not_Found_Error        : exception renames Uhppoted.Types.Event_Not_Found_Error;
   Event_Overwritten_Error      : exception renames Uhppoted.Types.Event_Overwritten_Error;
   Time_Profile_Not_Found_Error : exception renames Uhppoted.Types.Time_Profile_Not_Found_Error;
   Invalid_Response_Error       : exception renames Uhppoted.Types.Invalid_Response_Error;
   Timeout_Error                : exception renames Uhppoted.Types.Timeout_Error;

   function Find_Controllers (U       : UHPPOTE;
                              Timeout : Duration := 2.5) return Controller_Record_List;
   --  Finds all access controllers on the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          List of Controller_Record.

   function Get_Controller (U       : UHPPOTE;
                            C       : Unsigned_32;
                            Timeout : Duration := 2.5) return Controller_Record;
   --  Retrieves the information for a single access controller. Restricted to the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Controller_Record with the controller information.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Controller (U : UHPPOTE;
                            C : Controller;
                            Timeout : Duration := 2.5) return Controller_Record;
   --  Retrieves the information for a single access controller.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Controller_Record with the controller information.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_IPv4 (U       : UHPPOTE;
                      C       : Unsigned_32;
                      Addr    : GNAT.Sockets.Inet_Addr_Type;
                      Netmask : GNAT.Sockets.Inet_Addr_Type;
                      Gateway : GNAT.Sockets.Inet_Addr_Type;
                      Timeout : Duration := 2.5) return Boolean;
   --  Sets the access controller IPv4 address, subnet mask and gateway address. Restricted to the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number.
   --  @param  Addr     IPv4 address assigned to the controller.
   --  @param  Netmask  Network IPv4 subnet mask for the controller.
   --  @param  Gateway  Network gateway IPv4 address
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Controller_Record with the controller information.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_IPv4 (U       : UHPPOTE;
                      C       : Controller;
                      Addr    : GNAT.Sockets.Inet_Addr_Type;
                      Netmask : GNAT.Sockets.Inet_Addr_Type;
                      Gateway : GNAT.Sockets.Inet_Addr_Type;
                      Timeout : Duration := 2.5) return Boolean;
   --  Sets the access controller IPv4 address, subnet mask and gateway address.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Addr     IPv4 address assigned to the controller.
   --  @param  Netmask  Network IPv4 subnet mask for the controller.
   --  @param  Gateway  Network gateway IPv4 address
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          True if the controller network configuration was updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Time (U : UHPPOTE;
                      C : Unsigned_32;
                      Timeout : Duration := 2.5) return DateTime;
   --  Retrieves the access controller date/time. Restricted to the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          DateTime with the controller date/time.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Time (U : UHPPOTE;
                      C : Controller;
                      Timeout : Duration := 2.5) return DateTime;
   --  Retrieves the access controller date/time.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          DateTime with the controller date/time.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Time (U  : UHPPOTE;
                      C  : Unsigned_32;
                      DT : DateTime;
                      Timeout : Duration := 2.5) return DateTime;
   --  Sets the access controller date/time.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number.
   --  @param  DT       Date/time.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          DateTime with the controller date/time.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Time (U  : UHPPOTE;
                      C  : Controller;
                      DT : DateTime;
                      Timeout : Duration := 2.5) return DateTime;
   --  Sets the access controller date/time.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number, IPv4 address and (optional) procotol.
   --  @param  DT       Date/time.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          DateTime with the controller date/time.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Listener (U       : UHPPOTE;
                          C       : Unsigned_32;
                          Timeout : Duration := 2.5) return Listener_Record;
   --  Retrieves the access controller listener address:port and auto-send interval. Restricted to the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Listener_Record with the listener address:port and auto-send interval.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Listener (U       : UHPPOTE;
                          C       : Controller;
                          Timeout : Duration := 2.5) return Listener_Record;
   --  Retrieves the access controller listener address:port and auto-send interval.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Listener_Record with the listener address:port and auto-send interval.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Listener (U        : UHPPOTE;
                          C        : Unsigned_32;
                          Listener : GNAT.Sockets.Sock_Addr_Type;
                          Interval : Unsigned_8;
                          Timeout  : Duration := 2.5) return Boolean;
   --  Sets the access controller listener address:port and auto-send interval.
   --
   --  @param  U         UHPPOTE configuration.
   --  @param  C         Controller serial number.
   --  @param  Listener  Event listener address:port.
   --  @param  Interval  Auto-send interval (seconds). 0 for none.
   --  @param  Timeout   Operation timeout (defaults to 2.5s).
   --
   --  @return           True if the controller event listener and auto-send interval were configured.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Listener (U        : UHPPOTE;
                          C        : Controller;
                          Listener : GNAT.Sockets.Sock_Addr_Type;
                          Interval : Unsigned_8;
                          Timeout  : Duration := 2.5) return Boolean;
   --  Sets the access controller listener address:port and auto-send interval.
   --
   --  @param  U         UHPPOTE configuration.
   --  @param  C         Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Listener  Event listener address:port.
   --  @param  Interval  Auto-send interval (seconds). 0 for none.
   --  @param  Timeout   Operation timeout (defaults to 2.5s).
   --
   --  @return           True if the controller event listener and auto-send interval were configured.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Status (U : UHPPOTE;
                        C : Unsigned_32;
                        Timeout : Duration := 2.5) return Controller_Status;
   --  Retrieves the access controller status. Restricted to the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Controller_Status with the current controller state.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Status (U : UHPPOTE;
                        C : Controller;
                        Timeout : Duration := 2.5) return Controller_Status;
   --  Retrieves the access controller status.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Controller_Status with the current controller state.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Door (U       : UHPPOTE;
                      C       : Unsigned_32;
                      Door    : Unsigned_8;
                      Timeout : Duration := 2.5) return Door_Record;
   --  Retrieves a door control mode and open delay. Restricted to the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number.
   --  @param  Door     Door ID [1..4].
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Door_Record with the control mode and open delay.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Door (U       : UHPPOTE;
                      C       : Controller;
                      Door    : Unsigned_8;
                      Timeout : Duration := 2.5) return Door_Record;
   --  Retrieves a door control mode and open delay.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Door     Door ID [1..4].
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Door_Record with the control mode and open delay.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Door (U         : UHPPOTE;
                      C         : Unsigned_32;
                      Door      : Unsigned_8;
                      Mode      : Uhppoted.Lib.Control_Mode;
                      OpenDelay : Unsigned_8;
                      Timeout   : Duration := 2.5) return Door_Record;
   --  Sets a door control mode and open delay. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Door       Door ID [1..4].
   --  @param  Mode       Door control mode (Controlled, Normally_Open or Normally_Closed).
   --  @param  OpenDelay  Time  (seconds) door remains unlocked after swipe.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return          Door_Record with the control mode and open delay.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Door (U         : UHPPOTE;
                      C         : Controller;
                      Door      : Unsigned_8;
                      Mode      : Uhppoted.Lib.Control_Mode;
                      OpenDelay : Unsigned_8;
                      Timeout   : Duration := 2.5) return Door_Record;
   --  Sets a door control mode and open delay.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Door       Door ID [1..4].
   --  @param  Mode       Door control mode (Controlled, Normally_Open or Normally_Closed).
   --  @param  OpenDelay  Time  (seconds) door remains unlocked after swipe.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Door_Record with the control mode and open delay.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Door_Passcodes (U         : UHPPOTE;
                                C         : Unsigned_32;
                                Door      : Unsigned_8;
                                Passcodes : Uhppoted.Lib.Passcodes_List;
                                Timeout   : Duration := 2.5) return Boolean;
   --  Sets the supervisor override passcodes for a door. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Door       Door ID [1..4].
   --  @param  Passcodes  List of up to 4 passcodes ([0..999999]).
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Boolean if the passcodes were accepted.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Door_Passcodes (U         : UHPPOTE;
                                C         : Controller;
                                Door      : Unsigned_8;
                                Passcodes : Uhppoted.Lib.Passcodes_List;
                                Timeout   : Duration := 2.5) return Boolean;
   --  Sets the supervisor override passcodes for a door.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Door       Door ID [1..4].
   --  @param  Passcodes  List of up to 4 passcodes ([0..999999]).
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Boolean if the passcodes were accepted.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Open_Door (U       : UHPPOTE;
                       C       : Unsigned_32;
                       Door    : Unsigned_8;
                       Timeout : Duration := 2.5) return Boolean;
   --  Remotely unlocks a door. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Door       Door ID [1..4].
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the door was unlocked.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Open_Door (U       : UHPPOTE;
                       C       : Controller;
                       Door    : Unsigned_8;
                       Timeout : Duration := 2.5) return Boolean;
   --  Remotely unlocks a door.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Door       Door ID [1..4].
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the door was unlocked.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Cards (U       : UHPPOTE;
                       C       : Unsigned_32;
                       Timeout : Duration := 2.5) return Unsigned_32;
   --  Retrieves the number of cards stored on an access controller. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Total number of stored card records.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Cards (U       : UHPPOTE;
                       C       : Controller;
                       Timeout : Duration := 2.5) return Unsigned_32;
   --  Retrieves the number of cards stored on an access controller.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Total number of stored card records.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Card (U       : UHPPOTE;
                      C       : Unsigned_32;
                      Card    : Unsigned_32;
                      Timeout : Duration := 2.5) return Card_Record;
   --  Retrieves the card record for the requested card number. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Card       Card number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Card record for card number.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   --  @exception Card_Not_Found_Error   if there is no record at the index.

   function Get_Card (U       : UHPPOTE;
                      C       : Controller;
                      Card    : Unsigned_32;
                      Timeout : Duration := 2.5) return Card_Record;
   --  Retrieves the card record for the requested card number.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Card       Card number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Card record for card number.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   --  @exception Card_Not_Found_Error   if there is no record at the index.

   function Get_Card_At_Index (U       : UHPPOTE;
                               C       : Unsigned_32;
                               Index   : Unsigned_32;
                               Timeout : Duration := 2.5) return Card_Record;
   --  Retrieves the card record at the request index. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Index      Card record index.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Card record at the requested index.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   --  @exception Card_Not_Found_Error   if there is no record at the index.
   --  @exception Card_Deleted_Error     if the record at the index has been deleted.

   function Get_Card_At_Index (U       : UHPPOTE;
                               C       : Controller;
                               Index   : Unsigned_32;
                               Timeout : Duration := 2.5) return Card_Record;
   --  Retrieves the card record at the request index.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Index      Card record index.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Card record at the requested index.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   --  @exception Card_Not_Found_Error   if there is no record at the index.
   --  @exception Card_Deleted_Error     if the record at the index has been deleted.

   function Put_Card (U          : UHPPOTE;
                      C          : Unsigned_32;
                      Card       : Card_Record;
                      Timeout    : Duration := 2.5) return Boolean;
   --  Adds/updates a card record to the controller's stored. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Card       Card record.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the record was added or updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   --  @exception Card_Not_Found_Error   if there is no record at the index.

   function Put_Card (U          : UHPPOTE;
                      C          : Controller;
                      Card       : Card_Record;
                      Timeout    : Duration := 2.5) return Boolean;
   --  Adds/updates a card record to the controller's stored.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Card       Card record.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the record was added or updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   --  @exception Card_Not_Found_Error   if there is no record at the index.

   function Delete_Card (U       : UHPPOTE;
                         C       : Unsigned_32;
                         Card    : Unsigned_32;
                         Timeout : Duration := 2.5) return Boolean;
   --  Deletes the the card record for the requested card number. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Card       Card number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the card was deleted.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Delete_Card (U       : UHPPOTE;
                         C       : Controller;
                         Card    : Unsigned_32;
                         Timeout : Duration := 2.5) return Boolean;
   --  Deletes the the card record for the requested card number.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Card       Card number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the card was deleted.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Delete_All_Cards (U       : UHPPOTE;
                              C       : Unsigned_32;
                              Timeout : Duration := 2.5) return Boolean;
   --  Deletes all card records from the controller. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the card was deleted.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Delete_All_Cards (U       : UHPPOTE;
                              C       : Controller;
                              Timeout : Duration := 2.5) return Boolean;
   --  Deletes all card records from the controller.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the card was deleted.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Event (U       : UHPPOTE;
                       C       : Unsigned_32;
                       Index   : Unsigned_32;
                       Timeout : Duration := 2.5) return Controller_Event;
   --  Retrieves an event index from the controller. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Index      Downloaded event index.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Event record.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   --  @exception Event_Not_Found        if the event index does not match an existing record.
   --  @exception Event_Overwritten      if the event at the index has been overwritten.

   function Get_Event (U       : UHPPOTE;
                       C       : Controller;
                       Index   : Unsigned_32;
                       Timeout : Duration := 2.5) return Controller_Event;
   --  Retrieves an event index from the controller.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Index      Downloaded event index.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Event record.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   --  @exception Event_Not_Found        if the event index does not match an existing record.
   --  @exception Event_Overwritten      if the event at the index has been overwritten.

   function Get_Event_Index (U       : UHPPOTE;
                             C       : Unsigned_32;
                             Timeout : Duration := 2.5) return Unsigned_32;
   --  Retrieves the event index from the controller. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            The controller event index.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Event_Index (U       : UHPPOTE;
                             C       : Controller;
                             Timeout : Duration := 2.5) return Unsigned_32;
   --  Retrieves the event index from the controller.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            The controller event index.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Event_Index (U       : UHPPOTE;
                             C       : Unsigned_32;
                             Index   : Unsigned_32;
                             Timeout : Duration := 2.5) return Boolean;
   --  Sets the downloaded event index on the controller. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Index      Downloaded event index.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the event index was updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Event_Index (U       : UHPPOTE;
                             C       : Controller;
                             Index   : Unsigned_32;
                             Timeout : Duration := 2.5) return Boolean;
   --  Sets the downloaded event index on the controller.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Index      Downloaded event index.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the event index was updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Record_Special_Events (U       : UHPPOTE;
                                   C       : Unsigned_32;
                                   Enabled : Boolean;
                                   Timeout : Duration := 2.5) return Boolean;
   --  Enables/disables events for e.g. door open, door unlock, etc. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Enabled    Enables/disables special events.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the event index was updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Record_Special_Events (U       : UHPPOTE;
                                   C       : Controller;
                                   Enabled : Boolean;
                                   Timeout : Duration := 2.5) return Boolean;
   --  Enables/disables events for e.g. door open, door unlock, etc.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Enabled    Enables/disables special events.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the event index was updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Time_Profile (U       : UHPPOTE;
                              C       : Unsigned_32;
                              Profile : Unsigned_8;
                              Timeout : Duration := 2.5) return Time_Profile;
   --  Retrieves a stored time profile from the controller. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Profile    Time profile ID ([2..254]).
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Stored time profile.
   --
   --  @exception Time_Profile_Not_Found if the controller does not have a corresponding time profile.
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller/profile ID.

   function Get_Time_Profile (U       : UHPPOTE;
                              C       : Controller;
                              Profile : Unsigned_8;
                              Timeout : Duration := 2.5) return Time_Profile;
   --  Retrieves a stored time profile from the controller.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Profile    Time profile ID ([2..254]).
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Stored time profile.
   --
   --  @exception Time_Profile_Not_Found if the controller does not have a corresponding time profile.
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller/profile ID.

   function Set_Time_Profile (U          : UHPPOTE;
                              C          : Unsigned_32;
                              Profile_ID : Unsigned_8;
                              Profile    : Time_Profile;
                              Timeout    : Duration := 2.5) return Boolean;
   --  Adds/updates a time profile on a controller. Restricted to the local LAN.
   --
   --  @param  U           UHPPOTE configuration.
   --  @param  C           Controller serial number.
   --  @param  Profile_ID  Time profile ID ([2..254]).
   --  @param  Profile     Time profile record.
   --  @param  Timeout     Operation timeout (defaults to 2.5s).
   --
   --  @return             True if the profile was added or updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller/profile ID.

   function Set_Time_Profile (U          : UHPPOTE;
                              C          : Controller;
                              Profile_ID : Unsigned_8;
                              Profile    : Time_Profile;
                              Timeout    : Duration := 2.5) return Boolean;
   --  Adds/updates a time profile on a controller.
   --
   --  @param  U           UHPPOTE configuration.
   --  @param  C           Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Profile_ID  Time profile ID ([2..254]).
   --  @param  Profile     Time profile record.
   --  @param  Timeout     Operation timeout (defaults to 2.5s).
   --
   --  @return             True if the profile was added or updated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller/profile ID.

   function Clear_Time_Profiles (U          : UHPPOTE;
                                 C          : Unsigned_32;
                                 Timeout    : Duration := 2.5) return Boolean;
   --  Clears all time profiles stored on a controller. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the stored profiles were cleared.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller/profile ID.

   function Clear_Time_Profiles (U          : UHPPOTE;
                                 C          : Controller;
                                 Timeout    : Duration := 2.5) return Boolean;
   --  Clears all time profiles stored on a controller.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the stored profiles were cleared.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller/profile ID.

   function Add_Task (U       : UHPPOTE;
                      C       : Unsigned_32;
                      T       : Task_Record;
                      Timeout : Duration := 2.5) return Boolean;
   --  Creates a scheduled task assigned to a controller managed door. Restricted to the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number.
   --  @param  T        Task record.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          True if the task was added to the list of scheduled tasks.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller/profile ID.

   function Add_Task (U       : UHPPOTE;
                      C       : Controller;
                      T       : Task_Record;
                      Timeout : Duration := 2.5) return Boolean;
   --  Creates a scheduled task assigned to a controller managed door.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  C        Controller serial number, IPv4 address and (optional) procotol.
   --  @param  T        Task record.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          True if the task was added to the list of scheduled tasks.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller/profile ID.

   function Refresh_Task_List (U       : UHPPOTE;
                               C       : Unsigned_32;
                               Timeout : Duration := 2.5) return Boolean;
   --  Moves pending tasks and first-cards from the pending list to the active list. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if pending list for tasks and first-cards was flushed to the active list.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Refresh_Task_List (U       : UHPPOTE;
                               C       : Controller;
                               Timeout : Duration := 2.5) return Boolean;
   --  Moves pending tasks and first-cards from the pending list to the active list.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if pending list for tasks and first-cards was flushed to the active list.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Clear_Task_List (U       : UHPPOTE;
                             C       : Unsigned_32;
                             Timeout : Duration := 2.5) return Boolean;
   --  Clears all tasks from the scheduled tasks list. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the scheduled tasks list was cleared.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Clear_Task_List (U       : UHPPOTE;
                             C       : Controller;
                             Timeout : Duration := 2.5) return Boolean;
   --  Clears all tasks from the scheduled tasks list.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the scheduled tasks list was cleared.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_PC_Control (U       : UHPPOTE;
                            C       : Unsigned_32;
                            Enable  : Boolean;
                            Timeout : Duration := 2.5) return Boolean;
   --  Enables/disables remote access control - the access controller will revert to local access control
   --  management if no message is received from the host for 30 seconds. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Enable     Enables/disables remote access control.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if remote access control was enabled/disabled.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_PC_Control (U       : UHPPOTE;
                            C       : Controller;
                            Enable  : Boolean;
                            Timeout : Duration := 2.5) return Boolean;
   --  Enables/disables remote access control - the access controller will revert to local access control
   --  management if no message is received from the host for 30 seconds.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Enable     Enables/disables remote access control.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if remote access control was enabled/disabled.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Interlock (U         : UHPPOTE;
                           C         : Unsigned_32;
                           Interlock : Uhppoted.Lib.Interlock;
                           Timeout : Duration := 2.5) return Boolean;
   --  Sets the controller door interlock mode. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Interlock  Door interlock mode.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if door interlock mode was set.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Set_Interlock (U         : UHPPOTE;
                           C         : Controller;
                           Interlock : Uhppoted.Lib.Interlock;
                           Timeout : Duration := 2.5) return Boolean;
   --  Sets the controller door interlock mode.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Interlock  Door interlock mode.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if door interlock mode was set.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Activate_Keypads (U       : UHPPOTE;
                              C       : Unsigned_32;
                              Keypads : Uhppoted.Lib.Keypads;
                              Timeout : Duration := 2.5) return Boolean;
   --  Activates/deactivates the keypads associated with a controller door card reader. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Keypads    List [1..4] of keypads to be activated/deactivated.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the door readers were activated/deactivated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Activate_Keypads (U       : UHPPOTE;
                              C       : Controller;
                              Keypads : Uhppoted.Lib.Keypads;
                              Timeout : Duration := 2.5) return Boolean;
   --  Activates/deactivates the keypads associated with a controller door card reader.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Keypads    List [1..4] of keypads to be activated/deactivated.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if the door reader keypads were activated/deactivated.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Antipassback (U       : UHPPOTE;
                              C       : Unsigned_32;
                              Timeout : Duration := 2.5) return Antipassback;
   --  Retrieves the controller anti-passback setting. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Current anti-passback setting.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Get_Antipassback (U       : UHPPOTE;
                              C       : Controller;
                              Timeout : Duration := 2.5) return Antipassback;
   --  Retrieves the controller anti-passback setting.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            Current anti-passback setting.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Restore_Default_Parameters (U       : UHPPOTE;
                                        C       : Unsigned_32;
                                        Timeout : Duration := 2.5) return Boolean;
   --  Resets the controller to the manufacturer settings. Restricted to the local LAN.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if controller was reset to the manufacturer defaults.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   function Restore_Default_Parameters (U       : UHPPOTE;
                                        C       : Controller;
                                        Timeout : Duration := 2.5) return Boolean;
   --  Resets the controller to the manufacturer settings.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  C          Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Timeout    Operation timeout (defaults to 2.5s).
   --
   --  @return            True if controller was reset to the manufacturer defaults.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.

   procedure Listen (U : UHPPOTE; Handler : Event_Handler'Class; Cancel : Signal);
   --  Establishes a UDP connection to receive controller events.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  Listener   Event_Handler implementation.
   --  @param  Cancel     Cancel signal to terminate event listener.

   function Image (Addr : IPv4) return String renames Uhppoted.Types.Image;
   --  Returns a string representation of the given IPv4 address in dotted-decimal format (e.g., "192.168.1.1").
   --
   --  @param Addr The IPv4 address to be converted.
   --  @return A string containing the formatted IP address.
   --  @see Uhppoted.Types.Image

   function Image (MAC  : Hardware_Addr) return String renames Uhppoted.Types.Image;
   --  Returns a string representation of the given MAC address in hexadecimal format (e.g., "12:34:56:78:9a:bc").
   --
   --  @param MAC The MAC address to be converted.
   --  @return A string containing the formatted MAC address.
   --  @see Uhppoted.Types.Image

   function Image (D : DateOnly) return String renames Uhppoted.Types.Image;
   --  Returns a string representation of the given date in yyyy-mm-dd format (e.g., "2026-02-26").
   --
   --  @param D  The date to be converted.
   --  @return   A string containing the formatted date.
   --  @see Uhppoted.Types.Image

   function Image (DT : DateTime) return String renames Uhppoted.Types.Image;
   --  Returns a string representation of the given date/time in yyyy-mm-dd HH:mm:ssformat (e.g. "2026-02-26 15:23:45").
   --
   --  @param DT  The date/time to be converted.
   --  @return    A string containing the formatted date/time.
   --  @see Uhppoted.Types.Image

   function Image (V : Event_Direction) return String renames Uhppoted.Types.Image;
   --  Returns a string representation of an event direction enum.
   --
   --  @param V   The event direction to be converted.
   --  @return    IN, OUT or UNKNOWN
   --  @see Uhppoted.Types.Image

   function Image (T : HHmm) return String renames Uhppoted.Types.Image;
   --  Returns a string representation of an HHmm.
   --
   --  @param T   HHmm to be converted.
   --  @return    Time formatted as "HH:mm"
   --  @see Uhppoted.Types.Image
end Uhppoted.Lib;
