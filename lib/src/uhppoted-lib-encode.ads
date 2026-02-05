package Uhppoted.Lib.Encode is

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (ID : Unsigned_32) return Uhppoted.Lib.Packet;

end Uhppoted.Lib.Encode;
