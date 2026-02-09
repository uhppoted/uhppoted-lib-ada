package body Uhppoted.Lib.Encode is

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (ID : Unsigned_32) return Packet is
      Buffer : Packet := [others => 0];
   begin
      Buffer (1) := 16#17#;
      Buffer (2) := 16#94#;

      return Buffer;
   end Get_Controller;

end Uhppoted.Lib.Encode;
