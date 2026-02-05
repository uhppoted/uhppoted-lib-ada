package Uhppoted.Lib.Decode is

   --  Decodes a 64 byte get-controller response as a Controller record.
   function Get_Controller (Reply : Uhppoted.Lib.Packet)
      return Uhppoted.Lib.Controller;

private
   type BCD is array (Positive range <>) of Unsigned_8;
   function BCD_To_String (Bytes : BCD) return String;
end Uhppoted.Lib.Decode;
