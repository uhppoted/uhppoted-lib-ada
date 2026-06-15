with GNAT.Sockets;
with Uhppoted.Lib.Requests;

package body Uhppoted.Lib.Encode is
   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Requests;

   function Pack_IPv4 (Addr : Inet_Addr_Type) return IPv4;
   function Pack_DateTime (DT : DateTime) return BCD7;
   function Pack_Date (D : DateOnly) return BCD4;
   function Pack_HHmm (T : HHmm) return BCD2;
   function Pack_Boolean (B : Boolean) return Unsigned_8;

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (Controller : Unsigned_32) return Packet is
      Request : Get_Controller_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Controller;

   --  Encodes a set-IPv4 request as a 64 byte array.
   --!format off
   function Set_IPv4
     (Controller : Unsigned_32;
      Addr       : Inet_Addr_Type;
      Netmask    : Inet_Addr_Type;
      Gateway    : Inet_Addr_Type) return Packet
   is
      Request : Set_IPv4_Request;
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Addr := Pack_IPv4 (Addr);
      Request.Netmask := Pack_IPv4 (Netmask);
      Request.Gateway := Pack_IPv4 (Gateway);

      return Buffer;
   end Set_IPv4;

   --  Encodes a get-time request as a 64 byte array.
   function Get_Time (Controller : Unsigned_32) return Packet is
      Request : Get_Time_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Time;

   --  Encodes a set-time request as a 64 byte array.
   function Set_Time (Controller : Unsigned_32; DT : DateTime) return Packet is
      Request : Set_Time_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Date_Time := Pack_DateTime (DT);

      return Buffer;
   end Set_Time;

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener (Controller : Unsigned_32) return Packet is
      Request : Get_Listener_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Listener;

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener_Addr_Port (Controller : Unsigned_32) return Packet is
      Request : Get_Listener_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Listener_Addr_Port;

   --  Encodes a set-listener request as a 64 byte array.
   --!format off
   function Set_Listener (Controller : Unsigned_32;
                          Addr       : GNAT.Sockets.Inet_Addr_Type;
                          Port       : Unsigned_16;
                          Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      Request : Set_Listener_Request;
      Buffer  : Packet with Address => Request'Address;
   --!format on
   begin
      Request.Controller := Controller;
      Request.Addr := Pack_IPv4 (Addr);
      Request.Port := Port;
      Request.Interval := Interval;

      return Buffer;
   end Set_Listener;

   --  Encodes a set-listener request as a 64 byte array.
   --!format off
   function Set_Listener_Addr_Port (Controller : Unsigned_32;
                                    Listener   : GNAT.Sockets.Sock_Addr_Type;
                                    Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      Request : Set_Listener_Request;
      Buffer  : Packet with Address => Request'Address;
   --!format on
   begin
      Request.Controller := Controller;
      Request.Addr := Pack_IPv4 (Listener.Addr);
      Request.Port := Unsigned_16 (Listener.Port);
      Request.Interval := Interval;

      return Buffer;
   end Set_Listener_Addr_Port;

   --  Encodes a get-status request as a 64 byte array.
   function Get_Status (Controller : Unsigned_32) return Packet is
      Request : Get_Status_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Status;

   --  Encodes a get-door request as a 64 byte array.
   function Get_Door (Controller : Unsigned_32; Door : Unsigned_8) return Packet is
      Request : Get_Door_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Door := Door;

      return Buffer;
   end Get_Door;

   --  Encodes a set-door request as a 64 byte array.
   --!format off
   function Set_Door (Controller : Unsigned_32;
                      Door       : Unsigned_8;
                      Mode       : Uhppoted.Lib.Control_Mode;
                      OpenDelay  : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      --!format on
      Request : Set_Door_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Door := Door;
      Request.Mode := Mode;
      Request.OpenDelay := OpenDelay;

      return Buffer;
   end Set_Door;

   --  Encodes a set-door-passcodes request as a 64 byte array.
   function Set_Door_Passcodes
     (Controller : Unsigned_32;
      Door       : Unsigned_8;
      Passcode1  : Unsigned_32;
      Passcode2  : Unsigned_32;
      Passcode3  : Unsigned_32;
      Passcode4  : Unsigned_32) return Uhppoted.Lib.Types.Packet
   is
      Request : Set_Door_Passcodes_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Door := Door;
      Request.Passcode1 := 0;
      Request.Passcode2 := 0;
      Request.Passcode3 := 0;
      Request.Passcode4 := 0;

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
   function Open_Door (Controller : Unsigned_32; Door : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      Request : Open_Door_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Door := Door;

      return Buffer;
   end Open_Door;

   --  Encodes a get-cards request as a 64 byte array.
   function Get_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Cards_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Cards;

   --  Encodes a get-card request as a 64 byte array.
   function Get_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Card_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Card := Card;

      return Buffer;
   end Get_Card;

   --  Encodes a get-card-at-index request as a 64 byte array.
   function Get_Card_At_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Card_At_Index_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Index := Index;

      return Buffer;
   end Get_Card_At_Index;

   --  Encodes a put-card request as a 64 byte array.
   function Put_Card
     (Controller : Unsigned_32;
      Card       : Unsigned_32;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Door_1     : Unsigned_8;
      Door_2     : Unsigned_8;
      Door_3     : Unsigned_8;
      Door_4     : Unsigned_8;
      PIN        : Unsigned_24) return Uhppoted.Lib.Types.Packet
   is
      Request : Put_Card_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Card := Card;
      Request.Start_Date := Pack_Date (Start_Date);
      Request.End_Date := Pack_Date (End_Date);
      Request.Door_1 := Door_1;
      Request.Door_2 := Door_2;
      Request.Door_3 := Door_3;
      Request.Door_4 := Door_4;
      Request.PIN := PIN;

      return Buffer;
   end Put_Card;

   --  Encodes a delete-card request as a 64 byte array.
   function Delete_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Delete_Card_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Card := Card;

      return Buffer;
   end Delete_Card;

   --  Encodes a delete-cards request as a 64 byte array.
   function Delete_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Delete_Cards_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Delete_Cards;

   --  Encodes a get-event request as a 64 byte array.
   function Get_Event (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Event_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Index := Index;

      return Buffer;
   end Get_Event;

   --  Encodes a get-event-index request as a 64 byte array.
   function Get_Event_Index (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Event_Index_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Event_Index;

   --  Encodes a set-event-index request as a 64 byte array.
   function Set_Event_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Set_Event_Index_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Index := Index;

      return Buffer;
   end Set_Event_Index;

   --  Encodes a record-special-events request as a 64 byte array.
   function Record_Special_Events (Controller : Unsigned_32; Enabled : Boolean) return Uhppoted.Lib.Types.Packet is
      Request : Record_Special_Events_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Enabled := Pack_Boolean (Enabled);

      return Buffer;
   end Record_Special_Events;

   --  Encodes a get-time-profile request as a 64 byte array.
   function Get_Time_Profile (Controller : Unsigned_32; Profile : Unsigned_8) return Uhppoted.Lib.Types.Packet is
      Request : Get_Time_Profile_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Profile := Profile;

      return Buffer;
   end Get_Time_Profile;

   --  Encodes a set-time-profile request as a 64 byte array.
   function Set_Time_Profile
     (Controller      : Unsigned_32;
      Profile         : Unsigned_8;
      Start_Date      : DateOnly;
      End_Date        : DateOnly;
      Monday          : Boolean;
      Tuesday         : Boolean;
      Wednesday       : Boolean;
      Thursday        : Boolean;
      Friday          : Boolean;
      Saturday        : Boolean;
      Sunday          : Boolean;
      Segment_1_Start : HHmm;
      Segment_1_End   : HHmm;
      Segment_2_Start : HHmm;
      Segment_2_End   : HHmm;
      Segment_3_Start : HHmm;
      Segment_3_End   : HHmm;
      Linked_Profile  : Unsigned_8) return Uhppoted.Lib.Types.Packet
   is
      Request : Set_Time_Profile_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Profile := Profile;
      Request.Start_Date := Pack_Date (Start_Date);
      Request.End_Date := Pack_Date (End_Date);
      Request.Monday := Pack_Boolean (Monday);
      Request.Tuesday := Pack_Boolean (Tuesday);
      Request.Wednesday := Pack_Boolean (Wednesday);
      Request.Thursday := Pack_Boolean (Thursday);
      Request.Friday := Pack_Boolean (Friday);
      Request.Saturday := Pack_Boolean (Saturday);
      Request.Sunday := Pack_Boolean (Sunday);
      Request.Segment_1_Start := Pack_HHmm (Segment_1_Start);
      Request.Segment_1_End := Pack_HHmm (Segment_1_End);
      Request.Segment_2_Start := Pack_HHmm (Segment_2_Start);
      Request.Segment_2_End := Pack_HHmm (Segment_2_End);
      Request.Segment_3_Start := Pack_HHmm (Segment_3_Start);
      Request.Segment_3_End := Pack_HHmm (Segment_3_End);
      Request.Linked_Profile := Linked_Profile;

      return Buffer;
   end Set_Time_Profile;

   --  Encodes a clear-time-profiles request as a 64 byte array.
   function Clear_Time_Profiles (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Clear_Time_Profiles_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Clear_Time_Profiles;

   --  Encodes an add-task request as a 64 byte array.
   function Add_Task
     (Controller : Unsigned_32;
      Task_ID    : Task_Type;
      Start_Date : DateOnly;
      End_Date   : DateOnly;
      Monday     : Boolean;
      Tuesday    : Boolean;
      Wednesday  : Boolean;
      Thursday   : Boolean;
      Friday     : Boolean;
      Saturday   : Boolean;
      Sunday     : Boolean;
      Start_Time : HHmm;
      Door       : Unsigned_8;
      More_Cards : Unsigned_8) return Uhppoted.Lib.Types.Packet
   is
      Request : Add_Task_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Task_ID := Task_ID;
      Request.Start_Date := Pack_Date (Start_Date);
      Request.End_Date := Pack_Date (End_Date);
      Request.Monday := Pack_Boolean (Monday);
      Request.Tuesday := Pack_Boolean (Tuesday);
      Request.Wednesday := Pack_Boolean (Wednesday);
      Request.Thursday := Pack_Boolean (Thursday);
      Request.Friday := Pack_Boolean (Friday);
      Request.Saturday := Pack_Boolean (Saturday);
      Request.Sunday := Pack_Boolean (Sunday);
      Request.Start_Time := Pack_HHmm (Start_Time);
      Request.Door := Door;
      Request.More_Cards := More_Cards;

      return Buffer;
   end Add_Task;

   --  Encodes a refresh-tasklist request as a 64 byte array.
   function Refresh_Task_List (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Refresh_Task_List_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Refresh_Task_List;

   --  Encodes a clear-tasklist request as a 64 byte array.
   function Clear_Task_List (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Clear_Task_List_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Clear_Task_List;

   --  Encodes a set-pc-control request as a 64 byte array.
   function Set_PC_Control (Controller : Unsigned_32; Enable : Boolean) return Uhppoted.Lib.Types.Packet is
      Request : Set_PC_Control_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Enable := Pack_Boolean (Enable);

      return Buffer;
   end Set_PC_Control;

   --  Encodes a set-interlock request as a 64 byte array.
   --  !format off
   function Set_Interlock
     (Controller : Unsigned_32;
      Interlock  : Uhppoted.Lib.Interlock) return Uhppoted.Lib.Types.Packet
   --  !format on
   is
      Request : Set_Interlock_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;
      Request.Interlock := Interlock;

      return Buffer;
   end Set_Interlock;

   --  Encodes an activate-keypads request as a 64 byte array.
   --!format off
   function Activate_Keypads
     (Controller : Unsigned_32;
      Reader_1   : Boolean;
      Reader_2   : Boolean;
      Reader_3   : Boolean;
      Reader_4   : Boolean) return Uhppoted.Lib.Types.Packet
   is
      Request : Activate_Keypads_Request;
      Buffer  : Packet with Address => Request'Address;
   --!format on
   begin
      Request.Controller := Controller;
      Request.Reader_1 := Pack_Boolean (Reader_1);
      Request.Reader_2 := Pack_Boolean (Reader_2);
      Request.Reader_3 := Pack_Boolean (Reader_3);
      Request.Reader_4 := Pack_Boolean (Reader_4);

      return Buffer;
   end Activate_Keypads;

   --  Encodes a get-antipassback request as a 64 byte array.
   function Get_Antipassback (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Get_Antipassback_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Antipassback;

   --  Encodes a set-antipassback request as a 64 byte array.
   --!format off
   function Set_Antipassback
     (Controller   : Unsigned_32;
      Antipassback : Uhppoted.Lib.Antipassback) return Uhppoted.Lib.Types.Packet
   is
      Request : Set_Antipassback_Request;
      Buffer  : Packet with Address => Request'Address;
   --!format on
   begin
      Request.Controller := Controller;
      Request.Antipassback := Antipassback;

      return Buffer;
   end Set_Antipassback;

   --  Encodes a set-firstcard request as a 64 byte array.
   function Set_First_Card
     (Controller    : Unsigned_32;
      Door          : Unsigned_8;
      Start_Time    : HHmm;
      End_Time      : HHmm;
      Active_Mode   : Control_Mode;
      Inactive_Mode : Control_Mode;
      Monday        : Boolean;
      Tuesday       : Boolean;
      Wednesday     : Boolean;
      Thursday      : Boolean;
      Friday        : Boolean;
      Saturday      : Boolean;
      Sunday        : Boolean) return Uhppoted.Lib.Types.Packet
   is
      Request : Set_First_Card_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on

      Active   : Unsigned_8;
      Inactive : Unsigned_8;
   begin
      case Active_Mode is
         when Controlled      =>
            Active := 0;

         when Normally_Open   =>
            Active := 1;

         when Normally_Closed =>
            Active := 2;

         when others          =>
            raise Invalid_Door_Mode_Error;
      end case;

      case Inactive_Mode is
         when Controlled      =>
            Inactive := 0;

         when Normally_Open   =>
            Inactive := 1;

         when Normally_Closed =>
            Inactive := 2;

         when First_Card_Only =>
            Inactive := 3;
      end case;

      Request.Controller := Controller;
      Request.Door := Door;
      Request.Start_Time := Pack_HHmm (Start_Time);
      Request.End_Time := Pack_HHmm (End_Time);
      Request.Active_Mode := Active;
      Request.Inactive_Mode := Inactive;
      Request.Monday := Pack_Boolean (Monday);
      Request.Tuesday := Pack_Boolean (Tuesday);
      Request.Wednesday := Pack_Boolean (Wednesday);
      Request.Thursday := Pack_Boolean (Thursday);
      Request.Friday := Pack_Boolean (Friday);
      Request.Saturday := Pack_Boolean (Saturday);
      Request.Sunday := Pack_Boolean (Sunday);

      return Buffer;
   end Set_First_Card;

   --  Encodes a restore-default-parameters request as a 64 byte array.
   function Restore_Default_Parameters (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet is
      Request : Restore_Default_Parameters_Request;
      --!format off
      Buffer  : Packet with Address => Request'Address;
      --!format on
   begin
      Request.Controller := Controller;

      return Buffer;
   end Restore_Default_Parameters;

   --  Packs an IPv4 address into a 4 byte array.
   function Pack_IPv4 (Addr : Inet_Addr_Type) return IPv4 is
      V : constant IPv4 :=
        [Unsigned_8 (Addr.Sin_V4 (1)),
         Unsigned_8 (Addr.Sin_V4 (2)),
         Unsigned_8 (Addr.Sin_V4 (3)),
         Unsigned_8 (Addr.Sin_V4 (4))];
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

   --  Packs an HH:mm value into 2 bytes of BCD.
   function Pack_HHmm (T : HHmm) return BCD2 is
      V : BCD2 := [others => 0];

      HH : constant Unsigned_8 := T.Hour;
      MM : constant Unsigned_8 := T.Minute;
   begin
      V (1) := Shift_Left (HH / 10, 4) + (HH mod 10);
      V (2) := Shift_Left (MM / 10, 4) + (MM mod 10);

      return V;
   end Pack_HHmm;

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
