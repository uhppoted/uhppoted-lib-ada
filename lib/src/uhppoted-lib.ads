--  Package: Uhppoted_Lib
--  @summary
--  API library for the UHPPOTE access controllers.
--
--  @description
--  Implements the functions to manage UHPPOTE controllers over UDP or TCP/IP.

with Interfaces;
with GNAT.Sockets;
with Uhppoted.Types;

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
      Protocol : Protocol_Type := Default;
   end record;

   Invalid_Response_Error : exception;

   subtype Controller_Record      is Uhppoted.Types.Controller_Record;
   subtype Controller_Record_List is Uhppoted.Types.Controller_Record_List;
   subtype Controller_Status      is Uhppoted.Types.Controller_Status;
   subtype DateTime               is Uhppoted.Types.DateTime;
   subtype Listener_Record        is Uhppoted.Types.Listener_Record;
   subtype Door_Record            is Uhppoted.Types.Door_Record;

   subtype Control_Mode           is Uhppoted.Types.Control_Mode;

   Normally_Open   : Control_Mode renames Uhppoted.Types.Normally_Open;
   Normally_Closed : Control_Mode renames Uhppoted.Types.Normally_Closed;
   Controlled      : Control_Mode renames Uhppoted.Types.Controlled;

   --  Finds all access controllers on the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          List of Controller_Record.
   function Find_Controllers (U : UHPPOTE;
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
   function Get_Controller (U : UHPPOTE;
                            C : Unsigned_32;
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
   function Get_Listener (U : UHPPOTE;
                          C : Unsigned_32;
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
   function Get_Listener (U : UHPPOTE;
                          C : Controller;
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
   --  @param  Door     Door ID ([1..4]).
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
   --  @param  Door     Door ID ([1..4]).
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
   --  @param  Door       Door ID ([1..4]).
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
   --  @param  Door       Door ID ([1..4]).
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
   --  @param DT  The date/time  to be converted.
   --  @return    A string containing the formatted date/time.
   --  @see Uhppoted.Types.Image
   function Image (DT : DateTime) return String renames Uhppoted.Types.Image;

end Uhppoted.Lib;
