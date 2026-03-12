with Uhppoted.Lib.Types;
with Uhppoted.Lib.Encode;
with Uhppoted.Lib.Decode;
with Uhppoted.Lib.Transport.UDP;
with Uhppoted.Lib.Transport.TCP;
with Uhppoted.Lib.Responses;

package body Uhppoted.Lib is
   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Responses;

   --  Factory function to convert a controller ID to a Controller record for Dispatch.
   function To_Controller (C : Unsigned_32) return Controller is  (Controller'(ID => C, others => <>));

   --  Common handler to dispatch a request to a controller and return the response. Handles demuxing the
   --  controller transport/protocol options.
   function Dispatch (U        : UHPPOTE;
                      DestAddr : Sock_Addr_Type;
                      Request  : Packet;
                      Protocol : Protocol_Type;
                      Timeout  : Duration) return Packet;

   --  Finds all access controllers on the local LAN.
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

   --  Retrieves the information for a single access controller. Restricted to the local LAN.
   function Get_Controller (U       : UHPPOTE;
                            C       : Unsigned_32;
                            Timeout : Duration := 2.5) return Controller_Record is
   begin
      return Get_Controller (U, To_Controller (C), Timeout);
   end Get_Controller;

   --  Retrieves the information for a single access controller.
   function Get_Controller (U       : UHPPOTE;
                            C       : Controller;
                            Timeout : Duration := 2.5) return Controller_Record is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Controller (C.ID);
      Reply   : Packet;
      R       : Get_Controller_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Controller (Reply);

      if R.Controller /= C.ID then
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

   --  Sets the access controller IPv4 address, subnet mask and gateway address. Restricted to the local LAN.
   function Set_IPv4 (U       : UHPPOTE;
                      C       : Unsigned_32;
                      Addr    : Inet_Addr_Type;
                      Netmask : Inet_Addr_Type;
                      Gateway : Inet_Addr_Type;
                      Timeout : Duration := 2.5) return Boolean is
   begin
      return Set_IPv4 (U, To_Controller (C), Addr, Netmask, Gateway, Timeout);
   end Set_IPv4;

   --  Sets the access controller IPv4 address, subnet mask and gateway address (not restricted to the local LAN).
   function Set_IPv4 (U       : UHPPOTE;
                      C       : Controller;
                      Addr    : Inet_Addr_Type;
                      Netmask : Inet_Addr_Type;
                      Gateway : Inet_Addr_Type;
                      Timeout : Duration := 2.5) return Boolean is
      Request  : constant Packet := Uhppoted.Lib.Encode.Set_IPv4 (C.ID, Addr, Netmask, Gateway);
      Reply    : Packet;
      R        : Set_IPv4_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Set_IPv4 (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Set_IPv4;

   --  Retrieves the access controller date/time. Restricted to the local LAN.
   function Get_Time (U       : UHPPOTE;
                      C       : Unsigned_32;
                      Timeout : Duration := 2.5) return DateTime is
   begin
      return Get_Time (U, To_Controller (C), Timeout);
   end Get_Time;

   --  Retrieves the access controller date/time.
   function Get_Time (U       : UHPPOTE;
                      C       : Controller;
                      Timeout : Duration := 2.5) return DateTime is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Time (C.ID);
      Reply   : Packet;
      R       : Get_Time_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Time (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Date_Time;
   end Get_Time;

   --  Sets the access controller date/time. Restricted to the local LAN.
   function Set_Time (U       : UHPPOTE;
                      C       : Unsigned_32;
                      DT      : DateTime;
                      Timeout : Duration := 2.5) return DateTime is
   begin
      return Set_Time (U, To_Controller (C), DT, Timeout);
   end Set_Time;

   --  Sets the access controller date/time.
   function Set_Time (U       : UHPPOTE;
                      C       : Controller;
                      DT      : DateTime;
                      Timeout : Duration := 2.5) return DateTime is
      Request : constant Packet := Uhppoted.Lib.Encode.Set_Time (C.ID, DT);
      Reply   : Packet;
      R       : Set_Time_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Set_Time (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Date_Time;
   end Set_Time;

   --  Retrieves the access controller listener address:port and auto-send interval. Restricted to the local LAN.
   function Get_Listener (U : UHPPOTE;
                          C : Unsigned_32;
                          Timeout : Duration := 2.5) return Listener_Record is
   begin
      return Get_Listener (U, To_Controller (C), Timeout);
   end Get_Listener;

   --  Retrieves the access controller listener address:port and auto-send interval.
   function Get_Listener (U : UHPPOTE;
                          C : Controller;
                          Timeout : Duration := 2.5) return Listener_Record is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Listener (C.ID);
      Reply   : Packet;
      R       : Get_Listener_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Listener (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return (AddrPort => Network_Socket_Address (Addr => Inet_Addr (Image (R.Address)), Port => Port_Type (R.Port)),
              Interval => R.Interval);
   end Get_Listener;

   --  Retrieves the access controller status. Restricted to the local LAN.
   function Get_Status (U       : UHPPOTE;
                        C       : Unsigned_32;
                        Timeout : Duration := 2.5) return Controller_Status is
   begin
      return Get_Status (U, To_Controller (C), Timeout);
   end Get_Status;

   --  Retrieves the access controller status.
   function Get_Status (U       : UHPPOTE;
                        C       : Controller;
                        Timeout : Duration := 2.5) return Controller_Status is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Status (C.ID);
      Reply   : Packet;
      R       : Get_Status_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Status (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return (System_Date_Time => (Year   => R.System_Date.Year,
                                   Month  => R.System_Date.Month,
                                   Day    => R.System_Date.Day,
                                   Hour   => R.System_Time.Hour,
                                   Minute => R.System_Time.Minute,
                                   Second => R.System_Time.Second),
              Doors => [1 => (Open     => R.Door_1_Open,
                              Button   => R.Door_1_Button,
                              Unlocked => (R.Relays and 16#01#) = 16#01#),
                        2 => (Open => R.Door_2_Open,
                              Button => R.Door_2_Button,
                              Unlocked => (R.Relays and 16#02#) = 16#02#),
                        3 => (Open => R.Door_3_Open,
                              Button => R.Door_3_Button,
                              Unlocked => (R.Relays and 16#04#) = 16#04#),
                        4 => (Open => R.Door_4_Open,
                              Button => R.Door_4_Button,
                              Unlocked => (R.Relays and 16#08#) = 16#08#)],

              Alarms => (Flags       => Unsigned_8 (R.Inputs),
                         Fire        => (R.Inputs and 16#01#) = 16#01#,
                         Lock_Forced => (R.Inputs and 16#02#) = 16#02#),

             System_Error => R.System_Error,
             Special_Info => R.Special_Info,

             Event => (Index          => R.Event_Index,
                       Event          => R.Event_Type,
                       Timestamp      => R.Event_Timestamp,
                       Door           => R.Event_Door,
                       Direction      => R.Event_Direction,
                       Card           => R.Event_Card,
                       Access_Granted => R.Event_Access_Granted,
                       Reason         => R.Event_Reason));
   end Get_Status;

   --  Common handler to dispatch a request to a controller and return the response. Handles demuxing the
   --  controller transport/protocol options.
   function Dispatch (U        : UHPPOTE;
                      DestAddr : Sock_Addr_Type;
                      Request  : Packet;
                      Protocol : Protocol_Type;
                      Timeout  : Duration) return Packet is
   begin
      if Protocol = TCP and then DestAddr /= No_Sock_Addr then
         return Uhppoted.Lib.Transport.TCP.Send (U, DestAddr, Request, Timeout);
      elsif DestAddr /= No_Sock_Addr then
         return Uhppoted.Lib.Transport.UDP.SendTo (U, DestAddr, Request, Timeout);
      else
         return Uhppoted.Lib.Transport.UDP.BroadcastTo (U, Request, Timeout);
      end if;
   end Dispatch;

end Uhppoted.Lib;
