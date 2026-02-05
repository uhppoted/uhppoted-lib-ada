with Ada.Unchecked_Conversion;
with Ada.Streams;
with System;

package body Uhppoted.Lib.Decode is
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

   type GetControllerResponse is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : MAC_Address;
      Version  : BCD (1 .. 2);
      Date     : BCD (1 .. 8);
      Reserved : Ada.Streams.Stream_Element_Array (1 .. 28);
   end record;

   for GetControllerResponse use record
      ID at 0 range  32 ..  63;
      Address at 0 range 64 .. 95;
      Netmask at 0 range 96 .. 127;
      Gateway at 0 range 128 .. 159;
      MAC at 0 range 160 .. 207;
      Version at 0 range 208 .. 223;
      Date at 0 range 224 .. 287;
   end record;

   for GetControllerResponse'Bit_Order use System.Low_Order_First;
   for GetControllerResponse'Scalar_Storage_Order use System.Low_Order_First;

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
