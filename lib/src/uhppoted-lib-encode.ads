with Uhppoted.Lib.Types;

package Uhppoted.Lib.Encode is
   use Uhppoted.Lib.Types;

   function Get_Controller (Controller : Unsigned_32) return Packet;
   function Set_IPv4 (Controller : Unsigned_32;
                      Addr       : GNAT.Sockets.Inet_Addr_Type;
                      Netmask    : GNAT.Sockets.Inet_Addr_Type;
                      Gateway    : GNAT.Sockets.Inet_Addr_Type) return Packet;

end Uhppoted.Lib.Encode;
