with Uhppoted.Types;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Encode is

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-IPv4 request as a 64 byte array.
   function Set_IPv4 (Controller : Unsigned_32;
                      Addr       : GNAT.Sockets.Inet_Addr_Type;
                      Netmask    : GNAT.Sockets.Inet_Addr_Type;
                      Gateway    : GNAT.Sockets.Inet_Addr_Type) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-time request as a 64 byte array.
   function Get_Time (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-time request as a 64 byte array.
   function Set_Time (Controller : Unsigned_32;
                      DT         : Uhppoted.Types.DateTime) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-status request as a 64 byte array.
   function Get_Status (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

end Uhppoted.Lib.Encode;
