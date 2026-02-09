with Ada.Strings.Fixed;

with Uhppoted.Lib.Encode;
with Uhppoted.Lib.Decode;
with Uhppoted.Lib.UDP;
with Uhppoted.Lib.Types;

package body Uhppoted.Lib is
   use Uhppoted.Lib.Types;

   function Find_Controllers return Controller_List is
      Request  : constant Packet := Uhppoted.Lib.Encode.Get_Controller (0);
      Replies  : constant Packet_List := Uhppoted.Lib.UDP.Broadcast (Request);
      Response : Controller_List (1 .. Integer(Replies.Length));
      IX       : Positive := 1;
   begin
      for Reply of Replies loop
         declare
            C : Controller;
         begin
            C := Uhppoted.Lib.Decode.Get_Controller (Reply);
            Response (IX) := C;
            IX := IX + 1;
         end;
      end loop;

      return Response;
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

end Uhppoted.Lib;
