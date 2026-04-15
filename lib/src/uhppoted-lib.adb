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
      IX       : Positive             := 1;
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
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Listener_Addr_Port (C.ID);
      Reply   : Packet;
      R       : Get_Listener_Addr_Port_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Listener_Addr_Port (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return (Listener => R.Listener,
              Interval => R.Interval);
   end Get_Listener;

   --  Sets the access controller listener address:port and auto-send interval. Restricted to the local LAN.
   function Set_Listener (U        : UHPPOTE;
                          C        : Unsigned_32;
                          Listener : GNAT.Sockets.Sock_Addr_Type;
                          Interval : Unsigned_8;
                          Timeout  : Duration := 2.5) return Boolean is
   begin
      return Set_Listener (U, To_Controller (C), Listener, Interval, Timeout);
   end Set_Listener;

   --  Sets the access controller listener address:port and auto-send interval.
   function Set_Listener (U        : UHPPOTE;
                          C        : Controller;
                          Listener : GNAT.Sockets.Sock_Addr_Type;
                          Interval : Unsigned_8;
                          Timeout  : Duration := 2.5) return Boolean is
      Request : constant Packet := Uhppoted.Lib.Encode.Set_Listener_Addr_Port (C.ID, Listener, Interval);
      Reply   : Packet;
      R       : Set_Listener_Addr_Port_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Set_Listener_Addr_Port (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Set_Listener;

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

   --  Retrieves a door control mode and open delay. Restricted to the local LAN.
   function Get_Door (U       : UHPPOTE;
                      C       : Unsigned_32;
                      Door    : Unsigned_8;
                      Timeout : Duration := 2.5) return Door_Record is
   begin
      return Get_Door (U, To_Controller (C), Door, Timeout);
   end Get_Door;

   --  Retrieves a door control mode and open delay.
   function Get_Door (U       : UHPPOTE;
                      C       : Controller;
                      Door    : Unsigned_8;
                      Timeout : Duration := 2.5) return Door_Record is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Door (C.ID, Door);
      Reply   : Packet;
      R       : Get_Door_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Door (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return (Mode      => To_Control_Mode (R.Mode),
              OpenDelay => R.OpenDelay);
   end Get_Door;

   --  Sets a door control mode and open delay. Restricted to the local LAN.
   function Set_Door (U         : UHPPOTE;
                      C         : Unsigned_32;
                      Door      : Unsigned_8;
                      Mode      : Control_Mode;
                      OpenDelay : Unsigned_8;
                      Timeout   : Duration := 2.5) return Door_Record is
   begin
      return Set_Door (U, To_Controller (C), Door, Mode, OpenDelay, Timeout);
   end Set_Door;

   --  Sets a door control mode and open delay.
   function Set_Door (U         : UHPPOTE;
                      C         : Controller;
                      Door      : Unsigned_8;
                      Mode      : Control_Mode;
                      OpenDelay : Unsigned_8;
                      Timeout   : Duration := 2.5) return Door_Record is
      Request : constant Packet := Uhppoted.Lib.Encode.Set_Door (C.ID, Door, Mode, OpenDelay);
      Reply   : Packet;
      R       : Set_Door_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Set_Door (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return (Mode      => To_Control_Mode (R.Mode),
              OpenDelay => R.OpenDelay);
   end Set_Door;

   --  Sets the supervisor override passcodes for a door. Restricted to the local LAN.
   function Set_Door_Passcodes (U         : UHPPOTE;
                                C         : Unsigned_32;
                                Door      : Unsigned_8;
                                Passcodes : Passcodes_List;
                                Timeout   : Duration := 2.5) return Boolean is
   begin
      return Set_Door_Passcodes (U, To_Controller (C), Door, Passcodes, Timeout);
   end Set_Door_Passcodes;

   --  Sets the supervisor override passcodes for a door.
   function Set_Door_Passcodes (U         : UHPPOTE;
                                C         : Controller;
                                Door      : Unsigned_8;
                                Passcodes : Passcodes_List;
                                Timeout   : Duration := 2.5) return Boolean is
      Request   : Packet;
      Reply     : Packet;
      R         : Set_Door_Passcodes_Response;

      Passcode1 : Unsigned_32 := 0;
      Passcode2 : Unsigned_32 := 0;
      Passcode3 : Unsigned_32 := 0;
      Passcode4 : Unsigned_32 := 0;
   begin
      if Passcodes'Length > 0 then
         Passcode1 := Passcodes (Passcodes'First);
      end if;

      if Passcodes'Length > 1 then
         Passcode2 := Passcodes (Passcodes'First + 1);
      end if;

      if Passcodes'Length > 2 then
         Passcode3 := Passcodes (Passcodes'First + 2);
      end if;

      if Passcodes'Length > 3 then
         Passcode4 := Passcodes (Passcodes'First + 3);
      end if;

      Request := Uhppoted.Lib.Encode.Set_Door_Passcodes (C.ID, Door, Passcode1, Passcode2, Passcode3, Passcode4);
      Reply   := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R       := Uhppoted.Lib.Decode.Set_Door_Passcodes (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Set_Door_Passcodes;

   --  Remotely unlocks a door. Restricted to the local LAN.
   function Open_Door (U         : UHPPOTE;
                       C         : Unsigned_32;
                       Door      : Unsigned_8;
                       Timeout   : Duration := 2.5) return Boolean is
   begin
      return Open_Door (U, To_Controller (C), Door, Timeout);
   end Open_Door;

   --  Remotely unlocks a door.
   function Open_Door (U         : UHPPOTE;
                       C         : Controller;
                       Door      : Unsigned_8;
                       Timeout   : Duration := 2.5) return Boolean is
      Request : constant Packet := Uhppoted.Lib.Encode.Open_Door (C.ID, Door);
      Reply   : Packet;
      R       : Open_Door_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Open_Door (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Open_Door;

   --  Retrieves the number of cards stored on an access controller. Restricted to the local LAN.
   function Get_Cards (U         : UHPPOTE;
                       C         : Unsigned_32;
                       Timeout   : Duration := 2.5) return Unsigned_32 is
   begin
      return Get_Cards (U, To_Controller (C), Timeout);
   end Get_Cards;

   --  Retrieves the number of cards stored on an access controller.
   function Get_Cards (U         : UHPPOTE;
                       C         : Controller;
                       Timeout   : Duration := 2.5) return Unsigned_32 is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Cards (C.ID);
      Reply   : Packet;
      R       : Get_Cards_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Cards (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Cards;
   end Get_Cards;

   --  Retrieves the card record for the requested card number. Restricted to the local LAN.
   function Get_Card (U         : UHPPOTE;
                      C         : Unsigned_32;
                      Card      : Unsigned_32;
                      Timeout   : Duration := 2.5) return Card_Record is
   begin
      return Get_Card (U, To_Controller (C), Card, Timeout);
   end Get_Card;

   --  Retrieves the card record for the requested card number.
   function Get_Card (U         : UHPPOTE;
                      C         : Controller;
                      Card      : Unsigned_32;
                      Timeout   : Duration := 2.5) return Card_Record is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Card (C.ID, Card);
      Reply   : Packet;
      R       : Get_Card_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Card (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      if R.Card /= Card and then R.Card /= 0 then
         raise Invalid_Response_Error;
      end if;

      if R.Card = 0 then
         raise Card_Not_Found_Error;
      end if;

      return (Card       => R.Card,
              Start_Date => R.Start_Date,
              End_Date   => R.End_Date,
              Door_1     => R.Door_1,
              Door_2     => R.Door_2,
              Door_3     => R.Door_3,
              Door_4     => R.Door_4,
              PIN        => R.PIN);
   end Get_Card;

   --  Retrieves the card record at the requested index. Restricted to the local LAN.
   function Get_Card_At_Index (U         : UHPPOTE;
                               C         : Unsigned_32;
                               Index     : Unsigned_32;
                               Timeout   : Duration := 2.5) return Card_Record is
   begin
      return Get_Card_At_Index (U, To_Controller (C), Index, Timeout);
   end Get_Card_At_Index;

   --  Retrieves the card record at the requested index.
   function Get_Card_At_Index (U         : UHPPOTE;
                               C         : Controller;
                               Index     : Unsigned_32;
                               Timeout   : Duration := 2.5) return Card_Record is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Card_At_Index (C.ID, Index);
      Reply   : Packet;
      R       : Get_Card_At_Index_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Get_Card_At_Index (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      if R.Card = 0 then
         raise Card_Not_Found_Error;
      end if;

      if R.Card = 16#ffffffff# then
         raise Card_Deleted_Error;
      end if;

      return (Card       => R.Card,
              Start_Date => R.Start_Date,
              End_Date   => R.End_Date,
              Door_1     => R.Door_1,
              Door_2     => R.Door_2,
              Door_3     => R.Door_3,
              Door_4     => R.Door_4,
              PIN        => R.PIN);
   end Get_Card_At_Index;

   --  Adds/updates a card record stored on the controller. Restricted to the local LAN.
   function Put_Card (U         : UHPPOTE;
                      C         : Unsigned_32;
                      Card      : Card_Record;
                      Timeout   : Duration := 2.5) return Boolean is
   begin
      return Put_Card (U, To_Controller (C), Card, Timeout);
   end Put_Card;

   --  Adds/updates a card record stored on the controller.
   function Put_Card (U         : UHPPOTE;
                      C         : Controller;
                      Card      : Card_Record;
                      Timeout   : Duration := 2.5) return Boolean is
      Request : constant Packet := Uhppoted.Lib.Encode.Put_Card (C.ID,
                                                                 Card.Card,
                                                                 Card.Start_Date,
                                                                 Card.End_Date,
                                                                 Card.Door_1,
                                                                 Card.Door_2,
                                                                 Card.Door_3,
                                                                 Card.Door_4,
                                                                 Card.PIN);
      Reply   : Packet;
      R       : Put_Card_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Put_Card (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Put_Card;

   --  Deletes a card record stored on the controller. Restricted to the local LAN.
   function Delete_Card (U         : UHPPOTE;
                         C         : Unsigned_32;
                         Card      : Unsigned_32;
                         Timeout   : Duration := 2.5) return Boolean is
   begin
      return Delete_Card (U, To_Controller (C), Card, Timeout);
   end Delete_Card;

   --  Deletes a card record stored on the controller.
   function Delete_Card (U         : UHPPOTE;
                         C         : Controller;
                         Card      : Unsigned_32;
                         Timeout   : Duration := 2.5) return Boolean is
      Request : constant Packet := Uhppoted.Lib.Encode.Delete_Card (C.ID, Card);
      Reply   : Packet;
      R       : Delete_Card_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Delete_Card (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Delete_Card;

   --  Deletes all card records from the controller. Restricted to the local LAN.
   function Delete_All_Cards (U         : UHPPOTE;
                              C         : Unsigned_32;
                              Timeout   : Duration := 2.5) return Boolean is
   begin
      return Delete_All_Cards (U, To_Controller (C), Timeout);
   end Delete_All_Cards;

   --  Deletes all card records from the controller.
   function Delete_All_Cards (U         : UHPPOTE;
                              C         : Controller;
                              Timeout   : Duration := 2.5) return Boolean is
      Request : constant Packet := Uhppoted.Lib.Encode.Delete_Cards (C.ID);
      Reply   : Packet;
      R       : Delete_All_Cards_Response;
   begin
      Reply := Dispatch (U, C.DestAddr, Request, C.Protocol, Timeout);
      R     := Uhppoted.Lib.Decode.Delete_All_Cards (Reply);

      if R.Controller /= C.ID then
         raise Invalid_Response_Error;
      end if;

      return R.Ok;
   end Delete_All_Cards;

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
