with GNAT.Sockets;
with Uhppoted.Lib.Requests;

package body Uhppoted.Lib.Encode is
   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Requests;

   function Pack_IPv4     (Addr : Inet_Addr_Type) return IPv4;
   function Pack_DateTime (DT   : DateTime) return BCD7;

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

   --  Encodes a get-time request as a 64 byte array.
   function Get_Time (Controller : Unsigned_32) return Packet is
      Request : Get_Time_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Time;

   --  Encodes a set-time request as a 64 byte array.
   function Set_Time (Controller : Unsigned_32;
                      DT         : DateTime) return Packet is
      Request : Set_Time_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Date_Time  := Pack_DateTime (DT);

      return Buffer;
   end Set_Time;

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener (Controller : Unsigned_32) return Packet is
      Request : Get_Listener_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Listener;

   --  Encodes a get-status request as a 64 byte array.
   function Get_Status (Controller : Unsigned_32) return Packet is
      Request : Get_Status_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Status;

   --  Packs an IPv4 address into a 4 byte array.
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

   --  Packs a date/time value into 7 bytes of BCD.
   function Pack_DateTime (DT : DateTime) return BCD7 is
      V : BCD7 := [others => 0];

      CC : constant Unsigned_8 := Unsigned_8 (DT.Year / 100);
      YY : constant Unsigned_8 := Unsigned_8 (DT.Year mod 100);
      MM : constant Unsigned_8 := DT.Month;
      DD : constant Unsigned_8 := DT.Day;
      HH : constant Unsigned_8 := DT.Hour;
      NN : constant Unsigned_8 := DT.Minute;
      SS : constant Unsigned_8 := DT.Second;
   begin
      V (1) := Shift_Left (CC / 10, 4) + (CC mod 10);
      V (2) := Shift_Left (YY / 10, 4) + (YY mod 10);
      V (3) := Shift_Left (MM / 10, 4) + (MM mod 10);
      V (4) := Shift_Left (DD / 10, 4) + (DD mod 10);
      V (5) := Shift_Left (HH / 10, 4) + (HH mod 10);
      V (6) := Shift_Left (NN / 10, 4) + (NN mod 10);
      V (7) := Shift_Left (SS / 10, 4) + (SS mod 10);

      return V;
   end Pack_DateTime;

end Uhppoted.Lib.Encode;
