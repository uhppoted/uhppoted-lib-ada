with Ada.Unchecked_Conversion;

with Uhppoted.Lib.Responses;

package body Uhppoted.Lib.Decode is
   use Uhppoted.Lib.Responses;

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

   --  Translates a BCD coded version to a string.
   function Unpack_Version (Bytes : BCD) return String is
   begin
      return BCD_To_String (Bytes);
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

   --  Decodes a 64 byte get-controller response as a Controller record.
   function Get_Controller (Reply : Uhppoted.Lib.Packet)
      return Uhppoted.Lib.Controller is

      function Convert is new Ada.Unchecked_Conversion (
         Source => Uhppoted.Lib.Packet,
         Target => GetControllerResponse);

      Response : constant GetControllerResponse := Convert (Reply);
   begin
      return (ID       => Response.ID,
              Address  => Response.Address,
              Netmask  => Response.Netmask,
              Gateway  => Response.Gateway,
              MAC      => Response.MAC,
              Firmware => Unpack_Version (Response.Version),
              Date     => Unpack_Date (Response.Date));
   end Get_Controller;

   --  for Elem of b loop

end Uhppoted.Lib.Decode;
