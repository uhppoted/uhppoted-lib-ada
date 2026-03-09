with Uhppoted.Types;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Encode is
   function Get_Controller (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   function Set_IPv4 (Controller : Unsigned_32;
                      Addr       : GNAT.Sockets.Inet_Addr_Type;
                      Netmask    : GNAT.Sockets.Inet_Addr_Type;
                      Gateway    : GNAT.Sockets.Inet_Addr_Type) return Uhppoted.Lib.Types.Packet;

   function Get_Time (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   function Set_Time (Controller : Unsigned_32;
                      DT         : Uhppoted.Types.DateTime) return Uhppoted.Lib.Types.Packet;

   function Get_Status (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

end Uhppoted.Lib.Encode;
