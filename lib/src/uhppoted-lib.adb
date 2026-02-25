with Uhppoted.Lib.Types;
with Uhppoted.Lib.Encode;
with Uhppoted.Lib.Decode;
with Uhppoted.Lib.Transport.UDP;
with Uhppoted.Lib.Transport.TCP;
with Uhppoted.Lib.Responses;

package body Uhppoted.Lib is
   use GNAT.Sockets;
   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   function Dispatch (U        : UHPPOTE;
                      DestAddr : Sock_Addr_Type;
                      Request  : Packet;
                      Protocol : Protocol_Type;
                      Timeout  : Duration) return Packet;

   function Find_Controllers (U       : UHPPOTE;
                              Timeout : Duration := 2.5) return Controller_Record_List is
      Request  : constant Packet := Uhppoted.Lib.Encode.Get_Controller (0);
      Replies  : constant Packet_List := Uhppoted.Lib.Transport.UDP.Broadcast (U, Request, Timeout);
      Response : Controller_Record_List (1 .. Integer (Replies.Length));
      IX       : Positive := 1;
   begin
      for Reply of Replies loop
         declare
            R : Get_Controller_Response;
         begin
            R := Uhppoted.Lib.Decode.Get_Controller (Reply);
            Response (IX) := (
              ID       => R.Controller,
              Address  => R.IP_Address,
              Netmask  => R.Subnet_Mask,
              Gateway  => R.Gateway,
              MAC      => R.MAC_Address,
              Firmware => R.Version,
              Date     => R.Date);

            IX := IX + 1;
         end;
      end loop;

      return Response;
   end Find_Controllers;

   function Get_Controller (U : UHPPOTE;
                            C : Unsigned_32;
                            Timeout : Duration := 2.5) return Controller_Record is
      DestAddr : constant Sock_Addr_Type := U.Broadcast_Addr;
      Request  : constant Packet := Uhppoted.Lib.Encode.Get_Controller (C);
      Reply    : constant Packet := Uhppoted.Lib.Transport.UDP.SendTo (U, DestAddr, Request, Timeout);
      R        : constant Get_Controller_Response := Uhppoted.Lib.Decode.Get_Controller (Reply);
   begin
      if R.Controller /= C then
         raise Invalid_Response_Error;
      end if;

      return (
         ID       => R.Controller,
         Address  => R.IP_Address,
         Netmask  => R.Subnet_Mask,
         Gateway  => R.Gateway,
         MAC      => R.MAC_Address,
         Firmware => R.Version,
         Date     => R.Date);
   end Get_Controller;

   function Get_Controller (U : UHPPOTE;
                            C : Controller;
                            Timeout : Duration := 2.5) return Controller_Record is
      DestAddr : Sock_Addr_Type := U.Broadcast_Addr;
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Controller (C.Controller);
      Reply   : Packet;
      R       : Get_Controller_Response;
   begin
      if C.DestAddr /= No_Sock_Addr then
         DestAddr := C.DestAddr;
      end if;

      Reply := Dispatch (U, DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Controller (Reply);

      if R.Controller /= C.Controller then
         raise Invalid_Response_Error;
      end if;

      return (
         ID       => R.Controller,
         Address  => R.IP_Address,
         Netmask  => R.Subnet_Mask,
         Gateway  => R.Gateway,
         MAC      => R.MAC_Address,
         Firmware => R.Version,
         Date     => R.Date);
   end Get_Controller;

   function Set_IPv4 (U       : UHPPOTE;
                      C       : Unsigned_32;
                      Addr    : Inet_Addr_Type;
                      Netmask : Inet_Addr_Type;
                      Gateway : Inet_Addr_Type;
                      Timeout : Duration := 2.5) return Boolean is
      DestAddr : constant Sock_Addr_Type := U.Broadcast_Addr;
      Request  : constant Packet := Uhppoted.Lib.Encode.Set_IPv4 (C, Addr, Netmask, Gateway);
      Reply    : constant Packet := Uhppoted.Lib.Transport.UDP.SendTo (U, DestAddr, Request, Timeout);
      R        : constant Set_IPv4_Response := Uhppoted.Lib.Decode.Set_IPv4 (Reply);
   begin
      if R.Controller /= C then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Set_IPv4;

   function Set_IPv4 (U       : UHPPOTE;
                      C       : Controller;
                      Addr    : Inet_Addr_Type;
                      Netmask : Inet_Addr_Type;
                      Gateway : Inet_Addr_Type;
                      Timeout : Duration := 2.5) return Boolean is
      DestAddr : Sock_Addr_Type := U.Broadcast_Addr;
      Request  : constant Packet := Uhppoted.Lib.Encode.Set_IPv4 (C.Controller, Addr, Netmask, Gateway);
      Reply    : Packet;
      R        : Set_IPv4_Response;
   begin
      if C.DestAddr /= No_Sock_Addr then
         DestAddr := C.DestAddr;
      end if;

      Reply := Dispatch (U, DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Set_IPv4 (Reply);

      if R.Controller /= C.Controller then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Set_IPv4;

   function Dispatch (U        : UHPPOTE;
                      DestAddr : Sock_Addr_Type;
                      Request  : Packet;
                      Protocol : Protocol_Type;
                      Timeout  : Duration) return Packet is
   begin
      if Protocol = TCP and then DestAddr /= No_Sock_Addr then
         return Uhppoted.Lib.Transport.TCP.Send (U, DestAddr, Request, Timeout);
      else
         return Uhppoted.Lib.Transport.UDP.SendTo (U, DestAddr, Request, Timeout);
      end if;
   end Dispatch;

end Uhppoted.Lib;
