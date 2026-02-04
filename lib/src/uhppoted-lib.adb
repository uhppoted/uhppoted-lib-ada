with Ada.Strings.Fixed;

with Uhppoted.Lib.Encode;
with Uhppoted.Lib.Decode;
with Uhppoted.Lib.UDP;

package body Uhppoted.Lib is

   function Find_Controllers return Controller_List is
      Request  : constant Packet := Uhppoted.Lib.Encode.Find_Controllers;
      Response : Packet;
      C : Controller;
   begin
      Response := Uhppoted.Lib.UDP.Send (Request);
      C := Uhppoted.Lib.Decode.Get_Controller (Response);

      return (1 => C);
   end Find_Controllers;

   function Image (Addr : IPv4) return String is
      use Ada.Strings.Fixed;
   begin
      return Trim (Addr (1)'Image, Ada.Strings.Both) & "." &
             Trim (Addr (2)'Image, Ada.Strings.Both) & "." &
             Trim (Addr (3)'Image, Ada.Strings.Both) & "." &
             Trim (Addr (4)'Image, Ada.Strings.Both);
   end Image;

   function Image (Date : DateOnly) return String is
      use Ada.Strings.Fixed;

      YYYY : constant String := Trim (Date.Year'Image, Ada.Strings.Both);
      MM   : constant String := (if Date.Month < 10 then "0" else "") & Trim (Date.Month'Image, Ada.Strings.Both);
      DD   : constant String := (if Date.Day   < 10 then "0" else "") & Trim (Date.Day'Image, Ada.Strings.Both);
   begin
      return YYYY & "-" & MM & "-" & DD;
   end Image;

   function Image (MAC : MAC_Address) return String is
      use Ada.Strings.Fixed;

      function To_Hex (Value : Unsigned_8) return String is
         Hex_Digits : constant String := "0123456789abcdef";
      begin
         return (1 => Hex_Digits (Integer (Value / 16) + 1),
                 2 => Hex_Digits (Integer (Value mod 16) + 1));
      end To_Hex;

   begin
      return To_Hex (MAC (1)) & ":" &
             To_Hex (MAC (2)) & ":" &
             To_Hex (MAC (3)) & ":" &
             To_Hex (MAC (4)) & ":" &
             To_Hex (MAC (5)) & ":" &
             To_Hex (MAC (6));
   end Image;
end Uhppoted.Lib;
