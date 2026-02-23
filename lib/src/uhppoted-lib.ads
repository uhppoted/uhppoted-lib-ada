with Interfaces;
with GNAT.Sockets;
with Uhppoted.Types;

package Uhppoted.Lib is
   use Uhppoted.Types;

   type UHPPOTE is record
      Bind_Addr      : GNAT.Sockets.Sock_Addr_Type;
      Broadcast_Addr : GNAT.Sockets.Sock_Addr_Type;
      Listen_Addr    : GNAT.Sockets.Sock_Addr_Type;
      Debug          : Boolean;
   end record;

   type Protocol_Type is (Default, Connected_UDP, TCP);
   type Controller is record
      Controller : Interfaces.Unsigned_32;
      DestAddr   : GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.No_Sock_Addr;
      Protocol   : Protocol_Type := Default;
   end record;

   subtype Controller_Record is Uhppoted.Types.Controller_Record;
   subtype Controller_Record_List is Uhppoted.Types.Controller_Record_List;

   function Find_Controllers (
      U : UHPPOTE;
      Timeout : Duration := 2.5
   ) return Controller_Record_List;

   function Get_Controller (
      U : UHPPOTE;
      C : Interfaces.Unsigned_32;
      Timeout : Duration := 2.5
   ) return Controller_Record;

   function Get_Controller (
      U : UHPPOTE;
      C : Controller;
      Timeout : Duration := 2.5
   ) return Controller_Record;

   function Image (Addr : IPv4) return String renames Uhppoted.Types.Image;
   function Image (MAC : MAC_Address) return String renames Uhppoted.Types.Image;
   function Image (Date : DateOnly) return String renames Uhppoted.Types.Image;

end Uhppoted.Lib;
