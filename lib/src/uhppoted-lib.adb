with Uhppoted.Lib.Encode;
with Uhppoted.Lib.Decode;
with Uhppoted.Lib.UDP;
with Uhppoted.Lib.Types;
with Uhppoted.Lib.Responses;

package body Uhppoted.Lib is
   use Interfaces;
   use GNAT.Sockets;
   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   function Find_Controllers (
      U : UHPPOTE;
      Timeout : Duration := 2.5
   ) return Controller_Record_List is
      Request  : constant Packet := Uhppoted.Lib.Encode.Get_Controller (0);
      Replies  : constant Packet_List := Uhppoted.Lib.UDP.Broadcast (U, Request, Timeout);
      Response : Controller_Record_List (1 .. Integer (Replies.Length));
      IX       : Positive := 1;
   begin
      for Reply of Replies loop
         declare
            R : Get_Controller_Response;
         begin
            R := Uhppoted.Lib.Decode.Get_Controller (Reply);
            Response (IX) := (
              ID       => R.ID,
              Address  => R.Address,
              Netmask  => R.Netmask,
              Gateway  => R.Gateway,
              MAC      => R.MAC,
              Firmware => R.Firmware,
              Date     => R.Date);

            IX := IX + 1;
         end;
      end loop;

      return Response;
   end Find_Controllers;

   function Get_Controller (
      U : UHPPOTE;
      C : Unsigned_32;
      Timeout : Duration := 2.5
   ) return Controller_Record is
      DestAddr : constant Sock_Addr_Type := U.Broadcast_Addr;
      Request  : constant Packet := Uhppoted.Lib.Encode.Get_Controller (C);
      Reply    : constant Packet := Uhppoted.Lib.UDP.SendTo (U, DestAddr, Request, Timeout);
      R        : constant Get_Controller_Response := Uhppoted.Lib.Decode.Get_Controller (Reply);
   begin
      return (
         ID       => R.ID,
         Address  => R.Address,
         Netmask  => R.Netmask,
         Gateway  => R.Gateway,
         MAC      => R.MAC,
         Firmware => R.Firmware,
         Date     => R.Date);
   end Get_Controller;

   function Get_Controller (
      U : UHPPOTE;
      C : Controller;
      Timeout : Duration := 2.5
   ) return Controller_Record is
      DestAddr : Sock_Addr_Type := U.Broadcast_Addr;
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Controller (C.Controller);
      Reply   : Packet;
      R       : Get_Controller_Response;
   begin
      if C.DestAddr /= No_Sock_Addr then
         DestAddr := C.DestAddr;
      end if;

      Reply := Uhppoted.Lib.UDP.SendTo (U, DestAddr, Request, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Controller (Reply);

      return (
         ID       => R.ID,
         Address  => R.Address,
         Netmask  => R.Netmask,
         Gateway  => R.Gateway,
         MAC      => R.MAC,
         Firmware => R.Firmware,
         Date     => R.Date);
   end Get_Controller;

end Uhppoted.Lib;
