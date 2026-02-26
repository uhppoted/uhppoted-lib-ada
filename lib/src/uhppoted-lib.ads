--  Package: Uhppoted_Lib
--  API library for the UHPPOTE access controllers.

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
      Controller : Unsigned_32;
      DestAddr   : Sock_Addr_Type := No_Sock_Addr;
      Protocol   : Protocol_Type := Default;
   end record;

   Invalid_Response_Error : exception;

   subtype Controller_Record      is Uhppoted.Types.Controller_Record;
   subtype Controller_Record_List is Uhppoted.Types.Controller_Record_List;

   --  Finds all access controllers on the local LAN.
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          List of Controller_Record.
   function Find_Controllers (U : UHPPOTE;
                              Timeout : Duration := 2.5) return Controller_Record_List;

   --  Retrieves the information for a single access controller (on the local LAN).
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

   --  Sets the access controller IPv4 address, subnet mask and gateway address (on the local LAN).
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
   --  @param Date The date address to be converted.
   --  @return A string containing the formatted date.
   --  @see Uhppoted.Types.Image
   function Image (Date : DateOnly) return String renames Uhppoted.Types.Image;

end Uhppoted.Lib;
