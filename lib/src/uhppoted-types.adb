with Ada.Strings.Fixed;

package body Uhppoted.Types is
   use Ada.Strings;

   function To_Control_Mode (V : Unsigned_8) return Control_Mode is
   begin
      case V is
         when 1 => return Normally_Open;
         when 2 => return Normally_Closed;
         when 3 => return Controlled;
         when others => raise Constraint_Error with "Invalid Control Mode: " & V'Image;
      end case;
   end To_Control_Mode;

   function Image (Addr : IPv4) return String is
      use Ada.Strings.Fixed;
   begin
      return Trim (Addr (1)'Image, Both) & "." &
             Trim (Addr (2)'Image, Both) & "." &
             Trim (Addr (3)'Image, Both) & "." &
             Trim (Addr (4)'Image, Both);
   end Image;

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

   function Image (D : DateOnly) return String is
      use Ada.Strings.Fixed;

      YYYY : constant String := Trim (D.Year'Image, Ada.Strings.Both);
      MM   : constant String := (if D.Month < 10 then "0" else "") & Trim (D.Month'Image, Both);
      DD   : constant String := (if D.Day   < 10 then "0" else "") & Trim (D.Day'Image,   Both);
   begin
      return YYYY & "-" & MM & "-" & DD;
   end Image;

   function Image (T : TimeOnly) return String is
      use Ada.Strings.Fixed;

      HH : constant String := (if T.Hour   < 10 then "0" else "") & Trim (T.Hour'Image,   Both);
      MM : constant String := (if T.Minute < 10 then "0" else "") & Trim (T.Minute'Image, Both);
      SS : constant String := (if T.Second < 10 then "0" else "") & Trim (T.Second'Image, Both);
   begin
      return HH & ":" & MM & ":" & SS;
   end Image;

   function Image (DT : DateTime) return String is
      use Ada.Strings.Fixed;

      YYYY    : constant String := Trim (DT.Year'Image, Ada.Strings.Both);
      MM      : constant String := (if DT.Month  < 10 then "0" else "") & Trim (DT.Month'Image,  Both);
      DD      : constant String := (if DT.Day    < 10 then "0" else "") & Trim (DT.Day'Image,    Both);
      HH      : constant String := (if DT.Hour   < 10 then "0" else "") & Trim (DT.Hour'Image,   Both);
      Minutes : constant String := (if DT.Minute < 10 then "0" else "") & Trim (DT.Minute'Image, Both);
      SS      : constant String := (if DT.Second < 10 then "0" else "") & Trim (DT.Second'Image, Both);
   begin
      return YYYY & "-" & MM & "-" & DD & " " & HH & ":" & Minutes & ":" & SS;
   end Image;

end Uhppoted.Types;
