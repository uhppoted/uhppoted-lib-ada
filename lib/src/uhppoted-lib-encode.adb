with Ada.Unchecked_Conversion;
with GNAT.Sockets;
with Uhppoted.Lib.Requests;

package body Uhppoted.Lib.Encode is
   use GNAT.Sockets;
   use Uhppoted.Lib.Requests;

   function Pack_IPv4 (Addr : Inet_Addr_Type) return IPv4;

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (Controller : Unsigned_32) return Packet is
      Request : Get_Controller_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Controller;

   --  Encodes a set-IPv4 request as a 64 byte array.
   function Set_IPv4 (Controller : Unsigned_32;
                      Addr       : Inet_Addr_Type;
                      Netmask    : Inet_Addr_Type;
                      Gateway    : Inet_Addr_Type) return Packet is
      Request : Set_IPv4_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Addr       := Pack_IPv4 (Addr);
      Request.Netmask    := Pack_IPv4 (Netmask);
      Request.Gateway    := Pack_IPv4 (Gateway);
      Request.MagicWord  := 16#55AAAA55#;

      return Buffer;
   end Set_IPv4;

   --  Utility functions

   function Pack_IPv4 (Addr : Inet_Addr_Type) return IPv4 is
      V : constant IPv4 := [
         Unsigned_8 (Addr.Sin_V4 (1)),
         Unsigned_8 (Addr.Sin_V4 (2)),
         Unsigned_8 (Addr.Sin_V4 (3)),
         Unsigned_8 (Addr.Sin_V4 (4))
      ];
   begin
      return V;
   end Pack_IPv4;

end Uhppoted.Lib.Encode;
