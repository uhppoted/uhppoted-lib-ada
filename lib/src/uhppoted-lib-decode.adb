with Ada.Strings;
with Ada.Strings.Fixed;
with Uhppoted.Lib.Codec;

package body Uhppoted.Lib.Decode is
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Codec;
   use Uhppoted.Lib.Replies;
   use Uhppoted.Lib.Responses;

   --  Decodes a 64 byte get-controller reply as a Get_Controller_Response record.
   function Get_Controller (Reply : Packet) return Responses.Get_Controller_Response is
      R : Replies.Get_Controller_Response with Import, Address => Reply'Address;
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Controller then
         raise Invalid_Response_Error;
      end if;

      return (Controller  => R.Controller,
              IP_Address  => R.Address,
              Subnet_Mask => R.Netmask,
              Gateway     => R.Gateway,
              MAC_Address => R.MAC,
              Version     => Unpack_Version (R.Version),
              Date        => Unpack_Date (R.Date));
   end Get_Controller;

   --  Decodes a 64 byte set-IPv4 reply as a Set_IPv4_Response record.
   function Set_IPv4 (Reply : Packet) return Responses.Set_IPv4_Response is
      R : Replies.Set_IPv4_Response with Import, Address => Reply'Address;
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_IPv4 then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller,
              Ok         => Unpack_Boolean (R.Ok));
   end Set_IPv4;

   --  Decodes a 64 byte get-time reply as a Get_Time_Response record.
   function Get_Time (Reply : Packet) return Responses.Get_Time_Response is
      R : Replies.Get_Time_Response with Import, Address => Reply'Address;
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Time then
         raise Invalid_Response_Error;
      end if;

      return (Controller  => R.Controller,
              Date_Time   => Unpack_Date_Time (R.Date_Time));
   end Get_Time;

   --  Decodes a 64 byte get-time reply as a Get_Time_Response record.
   function Get_Listener (Reply : Packet) return Responses.Get_Listener_Response is
      R : Replies.Get_Listener_Response with Import, Address => Reply'Address;
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Listener then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller,
              Address    => R.Address,
              Port       => R.Port,
              Interval   => R.Interval);
   end Get_Listener;

   --  Decodes a 64 byte set-time reply as a Set_Time_Response record.
   function Set_Time (Reply : Packet) return Responses.Set_Time_Response is
      R : Replies.Set_Time_Response with Import, Address => Reply'Address;
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Time then
         raise Invalid_Response_Error;
      end if;

      return (Controller  => R.Controller,
              Date_Time   => Unpack_Date_Time (R.Date_Time));
   end Set_Time;

   --  Decodes a 64 byte get-status reply as a Get_Status_Response record.
   function Get_Status (Reply : Packet) return Responses.Get_Status_Response is
      R : Replies.Get_Status_Response with Import, Address => Reply'Address;
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Status then
         raise Invalid_Response_Error;
      end if;

      return (Controller           => R.Controller,
              System_Date          => Unpack_Short_Date (R.System_Date),
              System_Time          => Unpack_Time (R.System_Time),
              Door_1_Open          => Unpack_Boolean (R.Door_1_Open),
              Door_2_Open          => Unpack_Boolean (R.Door_2_Open),
              Door_3_Open          => Unpack_Boolean (R.Door_3_Open),
              Door_4_Open          => Unpack_Boolean (R.Door_4_Open),
              Door_1_Button        => Unpack_Boolean (R.Door_1_Button),
              Door_2_Button        => Unpack_Boolean (R.Door_2_Button),
              Door_3_Button        => Unpack_Boolean (R.Door_3_Button),
              Door_4_Button        => Unpack_Boolean (R.Door_4_Button),
              Relays               => Relay_State (R.Relays),
              Inputs               => Inputs_State (R.Inputs),
              System_Error         => R.System_Error,
              Special_Info         => R.Special_Info,
              Event_Index          => R.Event_Index,
              Event_Type           => R.Event_Type,
              Event_Access_Granted => Unpack_Boolean (R.Event_Access_Granted),
              Event_Door           => R.Event_Door,
              Event_Direction      => R.Event_Direction,
              Event_Card           => R.Event_Card,
              Event_Timestamp      => Unpack_Date_Time (R.Event_Timestamp),
              Event_Reason         => R.Event_Reason,
              Sequence_No          => R.Sequence_No);
   end Get_Status;

   --  Translates a BCD coded version to a vN.NN formatted string.
   function Unpack_Version (V : Version_Field) return Unbounded_String is
      N1 : constant Integer := Integer (Shift_Right (V.Major, 4) and 16#0F#);
      N2 : constant Integer := Integer (Shift_Right (V.Major, 0) and 16#0F#);
      N3 : constant Integer := Integer (Shift_Right (V.Minor, 4) and 16#0F#);
      N4 : constant Integer := Integer (Shift_Right (V.Minor, 0) and 16#0F#);

      Major : constant Integer := N1 * 10 + N2;
      Minor : constant Integer := N3 * 10 + N4;
   begin
      return To_Unbounded_String ("v" & Trim (Major'Image, Left) & "." & Trim (Minor'Image, Left));
   end Unpack_Version;

   --  Translates an Unsigned_8 into a Boolean - 1 is True, anything else is False.
   function Unpack_Boolean (B : Unsigned_8) return Boolean is
   begin
      if B = 1 then
         return True;
      else
         return False;
      end if;
   end Unpack_Boolean;

   --  Translates a BCD coded date to a DateOnly.
   function Unpack_Date (Bytes : BCD) return DateOnly is
      YYYYMMDD : constant String := BCD_To_String (Bytes);
      YYYY     : constant Unsigned_16 := Unsigned_16'Value (YYYYMMDD (1 .. 4));
      MM       : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD (5 .. 6));
      DD       : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD (7 .. 8));
   begin
      return (Year => YYYY, Month => MM, Day => DD);
   end Unpack_Date;

   --  Translates a BCD coded YYMMDD date to a DateOnly.
   function Unpack_Short_Date (Bytes : BCD) return DateOnly is
      YYMMDD : constant String := BCD_To_String (Bytes);
      YY     : constant Unsigned_8 := Unsigned_8'Value (YYMMDD (1 .. 2));
      MM     : constant Unsigned_8 := Unsigned_8'Value (YYMMDD (3 .. 4));
      DD     : constant Unsigned_8 := Unsigned_8'Value (YYMMDD (5 .. 6));
   begin
      return (Year => 2000 + Unsigned_16 (YY), Month => MM, Day => DD);
   end Unpack_Short_Date;

   --  Translates a BCD coded time to a TimeOnly.
   function Unpack_Time (Bytes : BCD) return TimeOnly is
      HHMMSS : constant String := BCD_To_String (Bytes);
      HH     : constant Unsigned_8 := Unsigned_8'Value (HHMMSS (1 .. 2));
      MM     : constant Unsigned_8 := Unsigned_8'Value (HHMMSS (3 .. 4));
      SS     : constant Unsigned_8 := Unsigned_8'Value (HHMMSS (5 .. 6));
   begin
      return (Hour => HH, Minute => MM, Second => SS);
   end Unpack_Time;

   --  Translates a BCD coded date/time to a DateTime.
   function Unpack_Date_Time (Bytes : BCD) return DateTime is
      YYYYMMDD_HHMMSS : constant String := BCD_To_String (Bytes);

      Year   : constant Unsigned_16 := Unsigned_16'Value (YYYYMMDD_HHMMSS (1 .. 4));
      Month  : constant Unsigned_8  := Unsigned_8'Value  (YYYYMMDD_HHMMSS (5 .. 6));
      Day    : constant Unsigned_8  := Unsigned_8'Value  (YYYYMMDD_HHMMSS (7 .. 8));
      Hour   : constant Unsigned_8  := Unsigned_8'Value  (YYYYMMDD_HHMMSS (9 .. 10));
      Minute : constant Unsigned_8  := Unsigned_8'Value  (YYYYMMDD_HHMMSS (11 .. 12));
      Second : constant Unsigned_8  := Unsigned_8'Value  (YYYYMMDD_HHMMSS (13 .. 14));
   begin
      return (Year => Year, Month => Month, Day => Day, Hour => Hour, Minute => Minute, Second => Second);
   end Unpack_Date_Time;

   --  Translates a BCD coded string in a byte array to a string.
   function BCD_To_String (Bytes : BCD) return String is
      Hex : constant String := "0123456789";
      S   : String (1 .. Bytes'Length * 2);
      I   : Positive := 1;
   begin
      for B of Bytes loop
         declare
            MSB : constant Integer := Integer (Shift_Right (B, 4));
            LSB : constant Integer := Integer (B and 16#0F#);
         begin
            S (I) := Hex (MSB + 1);
            S (I + 1) := Hex (LSB + 1);
            I := I + 2;
         end;
      end loop;

      return S;
   end BCD_To_String;

end Uhppoted.Lib.Decode;
