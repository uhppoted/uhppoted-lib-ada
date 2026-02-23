with Interfaces;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Uhppoted.Lib.Replies;

package body Uhppoted.Lib.Decode is
   use Interfaces;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Uhppoted.Lib.Replies;

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

   --  Translates a BCD coded date to a DateOnly.
   function Unpack_Date (Bytes : BCD) return DateOnly is
      YYYYMMDD : constant String := BCD_To_String (Bytes);
      Year : constant Unsigned_16 := Unsigned_16'Value (YYYYMMDD (1 .. 4));
      Month : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD (5 .. 6));
      Day : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD (7 .. 8));
   begin
      return (Year => Year, Month => Month, Day => Day);
   end Unpack_Date;

   --  Decodes a 64 byte get-controller response as a Controller_Record record.
   function Get_Controller (Reply : Packet) return Get_Controller_Response is
      Response : GetControllerResponse with Import, Address => Reply'Address;
   begin
      return (Controller  => Response.Controller,
              IP_Address  => Response.Address,
              Subnet_Mask => Response.Netmask,
              Gateway     => Response.Gateway,
              MAC_Address => Response.MAC,
              Version     => Unpack_Version (Response.Version),
              Date        => Unpack_Date (Response.Date));
   end Get_Controller;

   --  for Elem of b loop

end Uhppoted.Lib.Decode;
