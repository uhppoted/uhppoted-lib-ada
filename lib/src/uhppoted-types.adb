with Ada.Strings.Fixed;

package body Uhppoted.Types is
   function Image (Addr : IPv4) return String is
      use Ada.Strings.Fixed;
   begin
      return Trim (Addr (1)'Image, Ada.Strings.Both) & "." &
             Trim (Addr (2)'Image, Ada.Strings.Both) & "." &
             Trim (Addr (3)'Image, Ada.Strings.Both) & "." &
             Trim (Addr (4)'Image, Ada.Strings.Both);
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
      MM   : constant String := (if D.Month < 10 then "0" else "") & Trim (D.Month'Image, Ada.Strings.Both);
      DD   : constant String := (if D.Day   < 10 then "0" else "") & Trim (D.Day'Image, Ada.Strings.Both);
   begin
      return YYYY & "-" & MM & "-" & DD;
   end Image;

   function Image (DT : DateTime) return String is
      use Ada.Strings.Fixed;

      YYYY    : constant String := Trim (DT.Year'Image, Ada.Strings.Both);
      MM      : constant String := (if DT.Month  < 10 then "0" else "") & Trim (DT.Month'Image,  Ada.Strings.Both);
      DD      : constant String := (if DT.Day    < 10 then "0" else "") & Trim (DT.Day'Image,    Ada.Strings.Both);
      HH      : constant String := (if DT.Hour   < 10 then "0" else "") & Trim (DT.Hour'Image,   Ada.Strings.Both);
      Minutes : constant String := (if DT.Minute < 10 then "0" else "") & Trim (DT.Minute'Image, Ada.Strings.Both);
      SS      : constant String := (if DT.Second < 10 then "0" else "") & Trim (DT.Second'Image, Ada.Strings.Both);
   begin
      return YYYY & "-" & MM & "-" & DD & " " & HH & ":" & Minutes & ":" & SS;
   end Image;

end Uhppoted.Types;
