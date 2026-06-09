with Ada.Strings.Fixed;

package body Uhppoted.Types is
   use Ada.Strings;

   overriding
   procedure Initialize (S : in out Signal) is
   begin
      GNAT.Sockets.Create_Selector (S.Selector);
   end Initialize;

   overriding
   procedure Finalize (S : in out Signal) is
   begin
      GNAT.Sockets.Close_Selector (S.Selector);
   end Finalize;

   procedure Trigger (S : in out Signal) is
   begin
      GNAT.Sockets.Abort_Selector (S.Selector);
   end Trigger;

   function To_Control_Mode (V : Unsigned_8) return Control_Mode is
   begin
      case V is
         when 1      =>
            return Normally_Open;

         when 2      =>
            return Normally_Closed;

         when 3      =>
            return Controlled;

         when others =>
            raise Constraint_Error with "Invalid Control Mode: " & V'Image;
      end case;
   end To_Control_Mode;

   function To_Event_Type (V : Unsigned_8) return Event_Type is
   begin
      case V is
         when 1      =>
            return Swipe;

         when 2      =>
            return Door;

         when 3      =>
            return Alarm;

         when 255    =>
            return Overwritten;

         when others =>
            return Unknown;
      end case;
   end To_Event_Type;

   function To_Event_Direction (V : Unsigned_8) return Event_Direction is
   begin
      case V is
         when 1      =>
            return Direction_In;

         when 2      =>
            return Direction_Out;

         when others =>
            return Direction_Unknown;
      end case;
   end To_Event_Direction;

   function To_Event_Reason (V : Unsigned_8) return Event_Reason is
   begin
      case V is
         when 16#01# =>
            return SwipeOk;

         when 16#05# =>
            return Denied_PC_Control;

         when 16#06# =>
            return Denied_Not_Allowed;

         when 16#07# =>
            return Denied_Incorrect_PIN;

         when 16#08# =>
            return Denied_Anti_Passback;

         when 16#09# =>
            return Denied_More_Cards;

         when 16#0A# =>
            return Denied_First_Card_Required;

         when 16#0B# =>
            return Denied_Door_Normally_Closed;

         when 16#0C# =>
            return Denied_Door_InterLock;

         when 16#0D# =>
            return Denied_Time_Profile;

         when 16#0F# =>
            return Denied_Invalid_Timezone;

         when 16#12# =>
            return Denied_Invalid;

         when 16#14# =>
            return Push_Button_Ok;

         when 16#17# =>
            return Door_Open;

         when 16#18# =>
            return Door_Closed;

         when 16#19# =>
            return Supervisor_Override;

         when 16#1C# =>
            return Controller_Power_On;

         when 16#1D# =>
            return Controller_Reset;

         when 16#1E# =>
            return Push_Button_Disabled;

         when 16#1F# =>
            return Push_Button_Lock_Forced;

         when 16#20# =>
            return Push_Button_Offline;

         when 16#21# =>
            return Push_Button_Door_InterLock;

         when 16#22# =>
            return Threat;

         when 16#25# =>
            return Open_Too_Long;

         when 16#26# =>
            return Forced_Open;

         when 16#27# =>
            return Fire;

         when 16#28# =>
            return Forced_Close;

         when 16#29# =>
            return Theft_Prevention;

         when 16#2A# =>
            return Zone_24x7;

         when 16#2B# =>
            return Emergency_Call;

         when 16#2C# =>
            return Remote_Open_Door;

         when 16#2D# =>
            return Remote_Open_Door_USB;

         when others =>
            return Other;
      end case;
   end To_Event_Reason;

   function To_Task_Type (V : Unsigned_8) return Task_Type is
   begin
      case V is
         when 0      =>
            return Door_Controlled;

         when 1      =>
            return Door_Normally_Open;

         when 2      =>
            return Door_Normally_Closed;

         when 3      =>
            return Disable_Time_Profile;

         when 4      =>
            return Enable_Time_Profile;

         when 5      =>
            return Card_No_Password;

         when 6      =>
            return Card_In_Password;

         when 7      =>
            return Card_InOut_Password;

         when 8      =>
            return Enable_More_Cards;

         when 9      =>
            return Disable_More_Cards;

         when 10     =>
            return Trigger_Once;

         when 11     =>
            return Disable_PushButton;

         when 12     =>
            return Enable_PushButton;

         when others =>
            raise Constraint_Error with "Invalid Task type: " & V'Image;
      end case;
   end To_Task_Type;

   function To_Interlock (V : Unsigned_8) return Interlock is
   begin
      case V is
         when 0      =>
            return No_Interlock;

         when 1      =>
            return Interlock_12;

         when 2      =>
            return Interlock_34;

         when 3      =>
            return Interlock_12_34;

         when 4      =>
            return Interlock_123;

         when 8      =>
            return Interlock_1234;

         when others =>
            raise Constraint_Error with "Invalid interlock: " & V'Image;
      end case;
   end To_Interlock;

   function To_Antipassback (V : Unsigned_8) return Antipassback is
   begin
      case V is
         when 0      =>
            return No_Antipassback;

         when 1      =>
            return Readers_12_34;

         when 2      =>
            return Readers_13_24;

         when 3      =>
            return Readers_1_23;

         when 4      =>
            return Readers_1_234;

         when others =>
            raise Constraint_Error with "Invalid antipassback: " & V'Image;
      end case;
   end To_Antipassback;

   --  Returns the IPv4 address formatted as "n.n.n.n", e.g. "192.168.1.100".
   function Image (Addr : IPv4) return String is
      use Ada.Strings.Fixed;
   begin
      return
        Trim (Addr (1)'Image, Both)
        & "."
        & Trim (Addr (2)'Image, Both)
        & "."
        & Trim (Addr (3)'Image, Both)
        & "."
        & Trim (Addr (4)'Image, Both);
   end Image;

   --  Returns the MAC address formatted as "xx:xx:xx:xx:xx:xx".
   function Image (MAC : Hardware_Addr) return String is
      Hex : constant String := "0123456789abcdef";
      S   : String (1 .. 18);
      I   : Positive := 1;
   begin
      for B of MAC loop
         declare
            MSB : constant Integer := Integer (Shift_Right (B, 4));
            LSB : constant Integer := Integer (B and 16#0F#);
         begin
            S (I) := Hex (MSB + 1);
            S (I + 1) := Hex (LSB + 1);
            S (I + 2) := ':';
            I := I + 3;
         end;
      end loop;

      return S (1 .. 17);
   end Image;

   --  Returns the date value formatted as "yyyy-mm-dd" e.g. "2026-06-23".
   function Image (D : DateOnly) return String is
      use Ada.Strings.Fixed;

      YYYY : constant String := Trim (D.Year'Image, Ada.Strings.Both);
      MM   : constant String := (if D.Month < 10 then "0" else "") & Trim (D.Month'Image, Both);
      DD   : constant String := (if D.Day < 10 then "0" else "") & Trim (D.Day'Image, Both);
   begin
      return YYYY & "-" & MM & "-" & DD;
   end Image;

   --  Returns the time value formatted as "HH:mm:ss" e.g. "13:45:56".
   function Image (T : TimeOnly) return String is
      use Ada.Strings.Fixed;

      HH : constant String := (if T.Hour < 10 then "0" else "") & Trim (T.Hour'Image, Both);
      MM : constant String := (if T.Minute < 10 then "0" else "") & Trim (T.Minute'Image, Both);
      SS : constant String := (if T.Second < 10 then "0" else "") & Trim (T.Second'Image, Both);
   begin
      return HH & ":" & MM & ":" & SS;
   end Image;

   --  Returns the date/time value formatted as "yyyy-mm-dd HH:mm:ss" e.g. "2026-06-23 13:45:56".
   function Image (DT : DateTime) return String is
      use Ada.Strings.Fixed;

      YYYY    : constant String := Trim (DT.Year'Image, Ada.Strings.Both);
      MM      : constant String := (if DT.Month < 10 then "0" else "") & Trim (DT.Month'Image, Both);
      DD      : constant String := (if DT.Day < 10 then "0" else "") & Trim (DT.Day'Image, Both);
      HH      : constant String := (if DT.Hour < 10 then "0" else "") & Trim (DT.Hour'Image, Both);
      Minutes : constant String := (if DT.Minute < 10 then "0" else "") & Trim (DT.Minute'Image, Both);
      SS      : constant String := (if DT.Second < 10 then "0" else "") & Trim (DT.Second'Image, Both);
   begin
      return YYYY & "-" & MM & "-" & DD & " " & HH & ":" & Minutes & ":" & SS;
   end Image;

   --  Returns the event direction formatted as "IN" or "OUT".
   function Image (V : Event_Direction) return String is
   begin
      case V is
         when Direction_In  =>
            return "IN";

         when Direction_Out =>
            return "OUT";

         when others        =>
            return "UNKNOWN";
      end case;
   end Image;

   --  Returns the HHmm value formatted as "HH:mm" e.g. "13:45".
   function Image (T : HHmm) return String is
      use Ada.Strings.Fixed;

      HH : constant String := (if T.Hour < 10 then "0" else "") & Trim (T.Hour'Image, Both);
      MM : constant String := (if T.Minute < 10 then "0" else "") & Trim (T.Minute'Image, Both);
   begin
      return HH & ":" & MM;
   end Image;

end Uhppoted.Types;
