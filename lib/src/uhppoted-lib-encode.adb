with GNAT.Sockets;
with Uhppoted.Lib.Requests;

package body Uhppoted.Lib.Encode is
   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Requests;

   function Pack_IPv4     (Addr : Inet_Addr_Type) return IPv4;
   function Pack_DateTime (DT   : DateTime)       return BCD7;
   function Pack_Date     (D    : DateOnly)       return BCD4;
   function Pack_Boolean  (B    : Boolean)        return Unsigned_8;

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
      Request.MagicWord  := 16#55AA_AA55#;

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

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener_Addr_Port (Controller : Unsigned_32) return Packet is
      Request : Get_Listener_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Listener_Addr_Port;

   --  Encodes a set-listener request as a 64 byte array.
   function Set_Listener (Controller : Unsigned_32;
                          Addr       : GNAT.Sockets.Inet_Addr_Type;
                          Port       : Unsigned_16;
                          Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      Request : Set_Listener_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Addr       := Pack_IPv4 (Addr);
      Request.Port       := Port;
      Request.Interval   := Interval;

      return Buffer;
   end Set_Listener;

   --  Encodes a set-listener request as a 64 byte array.
   function Set_Listener_Addr_Port (Controller : Unsigned_32;
                                    Listener   : GNAT.Sockets.Sock_Addr_Type;
                                    Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      Request : Set_Listener_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Addr       := Pack_IPv4 (Listener.Addr);
      Request.Port       := Unsigned_16 (Listener.Port);
      Request.Interval   := Interval;

      return Buffer;
   end Set_Listener_Addr_Port;

   --  Encodes a get-status request as a 64 byte array.
   function Get_Status (Controller : Unsigned_32) return Packet is
      Request : Get_Status_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Status;

   --  Encodes a get-door request as a 64 byte array.
   function Get_Door (Controller : Unsigned_32; Door : Unsigned_8) return Packet is
      Request : Get_Door_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Door       := Door;

      return Buffer;
   end Get_Door;

   --  Encodes a set-door request as a 64 byte array.
   function Set_Door (Controller : Unsigned_32;
                      Door       : Unsigned_8;
                      Mode       : Uhppoted.Lib.Control_Mode;
                      OpenDelay  : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      Request : Set_Door_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Door       := Door;
      Request.Mode       := Mode;
      Request.OpenDelay  := OpenDelay;

      return Buffer;
   end Set_Door;

   --  Encodes a set-door-passcodes request as a 64 byte array.
   function Set_Door_Passcodes (Controller : Unsigned_32;
                                Door       : Unsigned_8;
                                Passcode1  : Unsigned_32;
                                Passcode2  : Unsigned_32;
                                Passcode3  : Unsigned_32;
                                Passcode4  : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Set_Door_Passcodes_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Door       := Door;
      Request.Passcode1  := 0;
      Request.Passcode2  := 0;
      Request.Passcode3  := 0;
      Request.Passcode4  := 0;

      if Passcode1 <= 999_999 then
         Request.Passcode1 := Passcode1;
      end if;

      if Passcode2 <= 999_999 then
         Request.Passcode2 := Passcode2;
      end if;

      if Passcode3 <= 999_999 then
         Request.Passcode3 := Passcode3;
      end if;

      if Passcode4 <= 999_999 then
         Request.Passcode4 := Passcode4;
      end if;

      return Buffer;
   end Set_Door_Passcodes;

   --  Encodes an open-door request as a 64 byte array.
   function Open_Door (Controller : Unsigned_32;
                       Door       : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      Request : Open_Door_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Door       := Door;

      return Buffer;
   end Open_Door;

   --  Encodes a get-cards request as a 64 byte array.
   function Get_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Cards_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Cards;

   --  Encodes a get-card request as a 64 byte array.
   function Get_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Card_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Card := Card;

      return Buffer;
   end Get_Card;

   --  Encodes a get-card-at-index request as a 64 byte array.
   function Get_Card_At_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Card_At_Index_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Index := Index;

      return Buffer;
   end Get_Card_At_Index;

   --  Encodes a put-card request as a 64 byte array.
   function Put_Card (Controller : Unsigned_32;
                      Card       : Unsigned_32;
                      Start_Date : DateOnly;
                      End_Date   : DateOnly;
                      Door_1     : Unsigned_8;
                      Door_2     : Unsigned_8;
                      Door_3     : Unsigned_8;
                      Door_4     : Unsigned_8;
                      PIN        : Unsigned_24) return Uhppoted.Lib.Types.Packet is
      Request : Put_Card_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Card       := Card;
      Request.Start_Date := Pack_Date (Start_Date);
      Request.End_Date   := Pack_Date (End_Date);
      Request.Door_1     := Door_1;
      Request.Door_2     := Door_2;
      Request.Door_3     := Door_3;
      Request.Door_4     := Door_4;
      Request.PIN        := PIN;

      return Buffer;
   end Put_Card;

   --  Encodes a delete-card request as a 64 byte array.
   function Delete_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Delete_Card_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Card := Card;

      return Buffer;
   end Delete_Card;

   --  Encodes a delete-cards request as a 64 byte array.
   function Delete_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Delete_Cards_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Delete_Cards;

   --  Encodes a get-event request as a 64 byte array.
   function Get_Event (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Event_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Index := Index;

      return Buffer;
   end Get_Event;

   --  Encodes a get-event-index request as a 64 byte array.
   function Get_Event_Index (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Event_Index_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Event_Index;

   --  Encodes a set-event-index request as a 64 byte array.
   function Set_Event_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Set_Event_Index_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Index := Index;
      Request.MagicWord  := 16#55AA_AA55#;

      return Buffer;
   end Set_Event_Index;

   --  Encodes a record-special-events request as a 64 byte array.
   function Record_Special_Events (Controller : Unsigned_32; Enabled : Boolean) return Uhppoted.Lib.Types.Packet is
      Request : Record_Special_Events_Request;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;
      Request.Enabled := Pack_Boolean (Enabled);

      return Buffer;
   end Record_Special_Events;

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

   --  Packs a date value into 4 bytes of BCD.
   function Pack_Date (D : DateOnly) return BCD4 is
      V : BCD4 := [others => 0];

      CC : constant Unsigned_8 := Unsigned_8 (D.Year / 100);
      YY : constant Unsigned_8 := Unsigned_8 (D.Year mod 100);
      MM : constant Unsigned_8 := D.Month;
      DD : constant Unsigned_8 := D.Day;
   begin
      V (1) := Shift_Left (CC / 10, 4) + (CC mod 10);
      V (2) := Shift_Left (YY / 10, 4) + (YY mod 10);
      V (3) := Shift_Left (MM / 10, 4) + (MM mod 10);
      V (4) := Shift_Left (DD / 10, 4) + (DD mod 10);

      return V;
   end Pack_Date;

   --  Packs a Boolean value into a single byte.
   function Pack_Boolean (B : Boolean) return Unsigned_8 is
   begin
      if B then
         return 1;
      else
         return 0;
      end if;
   end Pack_Boolean;

end Uhppoted.Lib.Encode;
