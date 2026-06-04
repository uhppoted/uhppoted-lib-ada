with Interfaces;
with GNAT.Sockets;
with Uhppoted.Types;

--  API library for the UHPPOTE access controllers.
--
--  Implements the functions required to manage UHPPOTE controllers over UDP or TCP/IP.
--
package Uhppoted.Lib is
   use Interfaces;
   use GNAT.Sockets;
   use Uhppoted.Types;

   --  (weird gnatdoc bug: blank lines above and below required or param/field tags are rejected)

   --  Container record for the operational configuration required to execute a command.
   --
   --  @field Bind_Addr      Local socket address::port used for the send socket. Defaults to INADDR_ANY.
   --  @field Broadcast_Addr IPv4 broadcast address::port used on the local LAN.
   --  @field Listen_Addr    Local socket address::port for controller events.
   --  @field Debug          Enables verbose logging and debugging output.
   type UHPPOTE is record
      Bind_Addr      : Sock_Addr_Type := (Addr => Any_Inet_Addr,       Family => Family_Inet, Port => 0);
      Broadcast_Addr : Sock_Addr_Type := (Addr => Broadcast_Inet_Addr, Family => Family_Inet, Port => 60000);
      Listen_Addr    : Sock_Addr_Type := (Addr => Any_Inet_Addr,       Family => Family_Inet, Port => 60001);
      Debug          : Boolean := False;
   end record;

   --  Transport type enum.
   --
   --  @enum Default  UDP broadcast.
   --  @enum UDP      Connected UDP socket.
   --  @enum TCP      TCP socket.
   type Transport_Type is (Default, UDP, TCP);

   --  Encapsulates the information required for communication over a connected UDP or TCP socket.
   --
   --  @field ID        Controller serial number.
   --  @field DestAddr  IPv4 address::port.
   --  @field Transport UDP or TCP (defaults to UDP).
   type Controller is record
      ID        : Unsigned_32;
      DestAddr  : Sock_Addr_Type := No_Sock_Addr;
      Transport : Transport_Type := Default;
   end record;

   --  Interface type for an event handler.
   type Event_Handler is interface;

   --  Interface definition for the handler for a Listen_Event.
   --
   --  @param  Self        Handler for event.
   --  @param  Controller  Controller serial number.
   --  @param  State       Controller current state.
   --  @param  Event       Current event information (if any).
   procedure On_Event (Self       : Event_Handler;
                       Controller : Unsigned_32;
                       State      : Controller_State;
                       Event      : Controller_Event) is abstract;

   --  Encapsulates the static information for a controller.
   subtype Controller_Record is Uhppoted.Types.Controller_Record;

   --  Encapsulates the list of controllers returned by Find_Controllers.
   subtype Controller_Record_List is Uhppoted.Types.Controller_Record_List;

   --  Encapsulates the controller current state and most recent event (if any).
   subtype Controller_Status is Uhppoted.Types.Controller_Status;

   --  Encapsulates the information for a controller state.
   subtype Controller_State is Uhppoted.Types.Controller_State;

   --  Encapsulates the information for a controller event.
   subtype Controller_Event is Uhppoted.Types.Controller_Event;

   --  Encapsulates the control information for a door.
   subtype Door_Record is Uhppoted.Types.Door_Record;

   --  Encapsulates the information for an access card.
   subtype Card_Record is Uhppoted.Types.Card_Record;

   --  Encapsulates the information for a Set/Get_Listener.
   subtype Listener_Record is Uhppoted.Types.Listener_Record;

   --  Encapsulates the information for an access time profile.
   subtype Time_Profile is Uhppoted.Types.Time_Profile;

   --  Encapsulates a time segment for a time profile.
   subtype Time_Segment is Uhppoted.Types.Time_Segment;

   --  Encapsulates the information for a scheduled task.
   subtype Task_Record is Uhppoted.Types.Task_Record;

   --  Encapsulates the configuration information for a door 'first-card' mode.
   subtype First_Card_Record is Uhppoted.Types.First_Card_Record;

   --  Encapsulates a list of keypads.
   subtype Keypads is Uhppoted.Types.Keypads;

   --  Encapsulates a signal to asynchronously cancel a Listen thread.
   subtype Signal is Uhppoted.Types.Signal;

   --  Triggers a signal to asynchronously cancel a Listen thread.
   --
   --  @param  S  Signal to trigger.
   procedure Trigger (S : in out Signal) renames Uhppoted.Types.Trigger;

   --  Type for yyyy-mm-dd HH:mm date/time.
   subtype DateTime is Uhppoted.Types.DateTime;

   --  Enum type for time of day (minute resolution).
   subtype HHmm is Uhppoted.Types.HHmm;

   --  Enum type for door control modes.
   subtype Control_Mode is Uhppoted.Types.Control_Mode;

   --  List of supervisor override codes.
   subtype Passcodes_List is Uhppoted.Types.Passcodes_List;

   --  Enum type for scheduled task type.
   subtype Task_Type is Uhppoted.Types.Task_Type;

   --  Enum type for interlock mode.
   subtype Interlock is Uhppoted.Types.Interlock;

   --  Enum type for anti-passback mode.
   subtype Antipassback is Uhppoted.Types.Antipassback;

   --  Enum for door control mode permanently unlocked.
   Normally_Open : Control_Mode renames Uhppoted.Types.Normally_Open;

   --  Enum for door control mode permanently locked.
   Normally_Closed : Control_Mode renames Uhppoted.Types.Normally_Closed;

   --  Enum for door control mode requires a valid card swipe.
   Controlled : Control_Mode renames Uhppoted.Types.Controlled;

   --  Enum for door control mode requires a valid 'first card' swipe.
   First_Card_Only : Control_Mode renames Uhppoted.Types.First_Card_Only;

   --  Enum to set door control mode to controlled.
   Door_Controlled : Task_Type renames Uhppoted.Types.Door_Controlled;

   --  Enum to set door control mode to normally open.
   Door_Normally_Open : Task_Type renames Uhppoted.Types.Door_Normally_Open;

   --  Enum to set door control mode to normally closed.
   Door_Normally_Closed : Task_Type renames Uhppoted.Types.Door_Normally_Closed;

   --  Enum to sisbles any time profiles assigned to the door.
   Disable_Time_Profile : Task_Type renames Uhppoted.Types.Disable_Time_Profile;

   --  Enum to snales any time profiles assigned to the door.
   Enable_Time_Profile : Task_Type renames Uhppoted.Types.Enable_Time_Profile;

   --  Enum to snales card swipe without a PIN.
   Card_No_Password : Task_Type renames Uhppoted.Types.Card_No_Password;

   --  Enum to snales PINs on an IN card swipe.
   Card_In_Password : Task_Type renames Uhppoted.Types.Card_In_Password;

   --  Enum to snales PINS on both IN and OUT card swipes.
   Card_InOut_Password : Task_Type renames Uhppoted.Types.Card_InOut_Password;

   --  Enum to snales 'more cards'.
   Enable_More_Cards : Task_Type renames Uhppoted.Types.Enable_More_Cards;

   --  Enum to sisbles 'more cards'.
   Disable_More_Cards : Task_Type renames Uhppoted.Types.Disable_More_Cards;

   --  Enum to snlcks the door.
   Trigger_Once : Task_Type renames Uhppoted.Types.Trigger_Once;

   --  Enum to sisbles the door pushbutton.
   Disable_PushButton : Task_Type renames Uhppoted.Types.Disable_PushButton;

   --  Enum to snales the door pushbutton.
   Enable_PushButton : Task_Type renames Uhppoted.Types.Enable_PushButton;

   --  Enum to disable door interlocks.
   No_Interlock : Interlock renames Uhppoted.Types.No_Interlock;

   --  Enum to interlock doors 1 and 2..
   Interlock_12 : Interlock renames Uhppoted.Types.Interlock_12;

   --  Enum to interlock doors 3 and 4.
   Interlock_34 : Interlock renames Uhppoted.Types.Interlock_34;

   --  Enum to interlock doors 1 and 2 and interlocks doors 3 and 4 (independently).
   Interlock_12_34 : Interlock renames Uhppoted.Types.Interlock_12_34;

   --  Enum to interlock doors 1, 2 and 3.
   Interlock_123 : Interlock renames Uhppoted.Types.Interlock_123;

   --  Enum to interlock doors 1, 2, 3 and 4.
   Interlock_1234 : Interlock renames Uhppoted.Types.Interlock_1234;

   --  Enum for no anti-passback.
   No_Antipassback : Antipassback renames Uhppoted.Types.No_Antipassback;

   --  Enum for anti-passback between reader 1 and reader 2 and also between reader 3 and
   --  reader 4 (independently).
   Readers_12_34 : Antipassback renames Uhppoted.Types.Readers_12_34;

   --  Enum for anti-passback between readers 1 or 3 and readers 2 or 4.
   Readers_13_24 : Antipassback renames Uhppoted.Types.Readers_13_24;

   --  Enum for anti-passback between reader 1 and readers 2 or 3.
   Readers_1_23 : Antipassback renames Uhppoted.Types.Readers_1_23;

   --  Enum for anti-passback between reader 1 and readers 2, 3 or 4.
   Readers_1_234 : Antipassback renames Uhppoted.Types.Readers_1_234;

   --  Custom error raised when the bind, broadcast or listen address is invalid.
   Invalid_Address_Error : exception renames Uhppoted.Types.Invalid_Address_Error;

   --  Custom error raised when the door control mode is not valid.
   Invalid_Door_Mode_Error : exception renames Uhppoted.Types.Invalid_Door_Mode_Error;

   --  Custom error raised when there is no card record at an index.
   Card_Not_Found_Error : exception renames Uhppoted.Types.Card_Not_Found_Error;

   --  Custom error raised when the card record at an index has been deleted.
   Card_Deleted_Error : exception renames Uhppoted.Types.Card_Deleted_Error;

   --  Custom error raised when there is no event record at an index.
   Event_Not_Found_Error : exception renames Uhppoted.Types.Event_Not_Found_Error;

   --  Custom error raised when the event record at an index has been overwritten.
   Event_Overwritten_Error : exception renames Uhppoted.Types.Event_Overwritten_Error;

   --  Custom error raised when there is no time profile record at an ID.
   Time_Profile_Not_Found_Error : exception renames Uhppoted.Types.Time_Profile_Not_Found_Error;

   --  Custom error raised when the response from a controller does not match the controller ID, is the incorrect
   --  response type or has the incorrect information.
   Invalid_Response_Error : exception renames Uhppoted.Types.Invalid_Response_Error;

   --  Custom error raised when a controller does respond within the time limit.
   Timeout_Error : exception renames Uhppoted.Types.Timeout_Error;

   --  Finds all access controllers on the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          List of Controller_Record.
   function Find_Controllers (U       : UHPPOTE;
                              Timeout : Duration := 2.5) return Controller_Record_List;

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
   function Get_Controller (U       : UHPPOTE;
                            C       : Unsigned_32;
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
   function Get_Controller (U : UHPPOTE;
                            C : Controller;
                            Timeout : Duration := 2.5) return Controller_Record;

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
                      C       : Unsigned_32;
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
   function Set_IPv4 (U       : UHPPOTE;
                      C       : Controller;
                      Addr    : GNAT.Sockets.Inet_Addr_Type;
                      Netmask : GNAT.Sockets.Inet_Addr_Type;
                      Gateway : GNAT.Sockets.Inet_Addr_Type;
                      Timeout : Duration := 2.5) return Boolean;

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
                      C : Unsigned_32;
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
   function Get_Time (U : UHPPOTE;
                      C : Controller;
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
                      C  : Unsigned_32;
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
   function Set_Time (U  : UHPPOTE;
                      C  : Controller;
                      DT : DateTime;
                      Timeout : Duration := 2.5) return DateTime;

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
                          C       : Unsigned_32;
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
   function Get_Listener (U       : UHPPOTE;
                          C       : Controller;
                          Timeout : Duration := 2.5) return Listener_Record;

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
                          C        : Unsigned_32;
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
   function Set_Listener (U        : UHPPOTE;
                          C        : Controller;
                          Listener : GNAT.Sockets.Sock_Addr_Type;
                          Interval : Unsigned_8;
                          Timeout  : Duration := 2.5) return Boolean;

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
                        C : Unsigned_32;
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
   function Get_Status (U : UHPPOTE;
                        C : Controller;
                        Timeout : Duration := 2.5) return Controller_Status;

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
                      C       : Unsigned_32;
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
   function Get_Door (U       : UHPPOTE;
                      C       : Controller;
                      Door    : Unsigned_8;
                      Timeout : Duration := 2.5) return Door_Record;

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
                      C         : Unsigned_32;
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
   function Set_Door (U         : UHPPOTE;
                      C         : Controller;
                      Door      : Unsigned_8;
                      Mode      : Uhppoted.Lib.Control_Mode;
                      OpenDelay : Unsigned_8;
                      Timeout   : Duration := 2.5) return Door_Record;

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
                                C         : Unsigned_32;
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
   function Set_Door_Passcodes (U         : UHPPOTE;
                                C         : Controller;
                                Door      : Unsigned_8;
                                Passcodes : Uhppoted.Lib.Passcodes_List;
                                Timeout   : Duration := 2.5) return Boolean;

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
                       C       : Unsigned_32;
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
   function Open_Door (U       : UHPPOTE;
                       C       : Controller;
                       Door    : Unsigned_8;
                       Timeout : Duration := 2.5) return Boolean;

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
                       C       : Unsigned_32;
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
   function Get_Cards (U       : UHPPOTE;
                       C       : Controller;
                       Timeout : Duration := 2.5) return Unsigned_32;

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
                      C       : Unsigned_32;
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
   function Get_Card (U       : UHPPOTE;
                      C       : Controller;
                      Card    : Unsigned_32;
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
                               C       : Unsigned_32;
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
   function Get_Card_At_Index (U       : UHPPOTE;
                               C       : Controller;
                               Index   : Unsigned_32;
                               Timeout : Duration := 2.5) return Card_Record;

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
                      C          : Unsigned_32;
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
   function Put_Card (U          : UHPPOTE;
                      C          : Controller;
                      Card       : Card_Record;
                      Timeout    : Duration := 2.5) return Boolean;

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
                         C       : Unsigned_32;
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
   function Delete_Card (U       : UHPPOTE;
                         C       : Controller;
                         Card    : Unsigned_32;
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
                              C       : Unsigned_32;
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
   function Delete_All_Cards (U       : UHPPOTE;
                              C       : Controller;
                              Timeout : Duration := 2.5) return Boolean;

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
                       C       : Unsigned_32;
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
   function Get_Event (U       : UHPPOTE;
                       C       : Controller;
                       Index   : Unsigned_32;
                       Timeout : Duration := 2.5) return Controller_Event;

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
                             C       : Unsigned_32;
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
   function Get_Event_Index (U       : UHPPOTE;
                             C       : Controller;
                             Timeout : Duration := 2.5) return Unsigned_32;

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
                             C       : Unsigned_32;
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
   function Set_Event_Index (U       : UHPPOTE;
                             C       : Controller;
                             Index   : Unsigned_32;
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
                                   C       : Unsigned_32;
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
   function Record_Special_Events (U       : UHPPOTE;
                                   C       : Controller;
                                   Enabled : Boolean;
                                   Timeout : Duration := 2.5) return Boolean;

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
                              C       : Unsigned_32;
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
   function Get_Time_Profile (U       : UHPPOTE;
                              C       : Controller;
                              Profile : Unsigned_8;
                              Timeout : Duration := 2.5) return Time_Profile;

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
                              C          : Unsigned_32;
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
   function Set_Time_Profile (U          : UHPPOTE;
                              C          : Controller;
                              Profile_ID : Unsigned_8;
                              Profile    : Time_Profile;
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
                                 C          : Unsigned_32;
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
   function Clear_Time_Profiles (U          : UHPPOTE;
                                 C          : Controller;
                                 Timeout    : Duration := 2.5) return Boolean;

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
                      C       : Unsigned_32;
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
   function Add_Task (U       : UHPPOTE;
                      C       : Controller;
                      T       : Task_Record;
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
                               C       : Unsigned_32;
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
   function Refresh_Task_List (U       : UHPPOTE;
                               C       : Controller;
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
                             C       : Unsigned_32;
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
   function Clear_Task_List (U       : UHPPOTE;
                             C       : Controller;
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
                            C       : Unsigned_32;
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
   function Set_PC_Control (U       : UHPPOTE;
                            C       : Controller;
                            Enable  : Boolean;
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
                           C         : Unsigned_32;
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
   function Set_Interlock (U         : UHPPOTE;
                           C         : Controller;
                           Interlock : Uhppoted.Lib.Interlock;
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
                              C       : Unsigned_32;
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
   function Activate_Keypads (U       : UHPPOTE;
                              C       : Controller;
                              Keypads : Uhppoted.Lib.Keypads;
                              Timeout : Duration := 2.5) return Boolean;

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
                              C       : Unsigned_32;
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
   function Get_Antipassback (U       : UHPPOTE;
                              C       : Controller;
                              Timeout : Duration := 2.5) return Antipassback;

   --  Sets the controller anti-passback setting. Restricted to the local LAN.
   --
   --  @param  U              UHPPOTE configuration.
   --  @param  C              Controller serial number.
   --  @param  Anti_Passback  Anti-passback setting.
   --  @param  Timeout        Operation timeout (defaults to 2.5s).
   --
   --  @return                True if the controller anti-passback setting was accepted.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   function Set_Antipassback (U             : UHPPOTE;
                              C             : Unsigned_32;
                              Anti_Passback : Antipassback;
                              Timeout       : Duration := 2.5) return Boolean;

   --  Sets the controller anti-passback setting.
   --
   --  @param  U              UHPPOTE configuration.
   --  @param  C              Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Anti_Passback  Anti-passback setting.
   --  @param  Timeout        Operation timeout (defaults to 2.5s).
   --
   --  @return                True if the controller anti-passback setting was accepted.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   function Set_Antipassback (U             : UHPPOTE;
                              C             : Controller;
                              Anti_Passback : Antipassback;
                              Timeout       : Duration := 2.5) return Boolean;

   --  Sets the first-card mode for a controller controller managed door. Restricted to the local LAN.
   --
   --  @param  U           UHPPOTE configuration.
   --  @param  C           Controller serial number.
   --  @param  Door        Door ID [1..4]
   --  @param  First_Card  Door first-card configuration.
   --  @param  Timeout     Operation timeout (defaults to 2.5s).
   --
   --  @return             True if the first-card configuration was accepted. The configuration will take
   --                      effect after a subsequent Refresh_Tasks command.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   function Set_First_Card (U          : UHPPOTE;
                            C          : Unsigned_32;
                            Door       : Unsigned_8;
                            First_Card : First_Card_Record;
                            Timeout    : Duration := 2.5) return Boolean;

   --  Sets the first-card mode for a controller controller managed door.
   --
   --  @param  U           UHPPOTE configuration.
   --  @param  C           Controller serial number, IPv4 address and (optional) procotol.
   --  @param  Door        Door ID [1..4]
   --  @param  First_Card  Door first-card configuration.
   --  @param  Timeout     Operation timeout (defaults to 2.5s).
   --
   --  @return             True if the first-card configuration was accepted. The configuration will take
   --                      effect after a subsequent Refresh_Tasks command.
   --
   --  @exception Timeout_Error          if the controller did not respond.
   --  @exception Invalid_Response_Error if the response did not match the requested controller.
   function Set_First_Card (U          : UHPPOTE;
                            C          : Controller;
                            Door       : Unsigned_8;
                            First_Card : First_Card_Record;
                            Timeout    : Duration := 2.5) return Boolean;

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
                                        C       : Unsigned_32;
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
   function Restore_Default_Parameters (U       : UHPPOTE;
                                        C       : Controller;
                                        Timeout : Duration := 2.5) return Boolean;

   --  Establishes a UDP connection to receive controller events.
   --
   --  @param  U          UHPPOTE configuration.
   --  @param  Handler    Event_Handler implementation.
   --  @param  Cancel     Cancel signal to terminate event listener.
   procedure Listen (U : UHPPOTE; Handler : Event_Handler'Class; Cancel : Signal);

   --  Returns a string representation of the given IPv4 address in dotted-decimal format (e.g., "192.168.1.1").
   --
   --  @param Addr The IPv4 address to be converted.
   --  @return A string containing the formatted IP address.
   --  @see Uhppoted.Types.Image
   function Image (Addr : IPv4) return String renames Uhppoted.Types.Image;

   --  Returns a string representation of the given MAC address in hexadecimal format (e.g., "12:34:56:78:9a:bc").
   --
   --  @param MAC The MAC address to be converted.
   --  @return A string containing the formatted MAC address.
   --  @see Uhppoted.Types.Image
   function Image (MAC  : Hardware_Addr) return String renames Uhppoted.Types.Image;

   --  Returns a string representation of the given date in yyyy-mm-dd format (e.g., "2026-02-26").
   --
   --  @param D  The date to be converted.
   --  @return   A string containing the formatted date.
   --  @see Uhppoted.Types.Image
   function Image (D : DateOnly) return String renames Uhppoted.Types.Image;

   --  Returns a string representation of the given date/time in yyyy-mm-dd HH:mm:ssformat (e.g. "2026-02-26 15:23:45").
   --
   --  @param DT  The date/time to be converted.
   --  @return    A string containing the formatted date/time.
   --  @see Uhppoted.Types.Image
   function Image (DT : DateTime) return String renames Uhppoted.Types.Image;

   --  Returns a string representation of an event direction enum.
   --
   --  @param V   The event direction to be converted.
   --  @return    IN, OUT or UNKNOWN
   --  @see Uhppoted.Types.Image
   function Image (V : Event_Direction) return String renames Uhppoted.Types.Image;

   --  Returns a string representation of an HHmm.
   --
   --  @param T   HHmm to be converted.
   --  @return    Time formatted as "HH:mm"
   --  @see Uhppoted.Types.Image
   function Image (T : HHmm) return String renames Uhppoted.Types.Image;

end Uhppoted.Lib;
