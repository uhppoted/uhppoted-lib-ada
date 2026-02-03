package body Uhppoted.Lib.Encode is

   --  Encodes a find-controllers request as a 64 byte array.
   function Find_Controllers return Uhppoted.Lib.Packet is
      Buffer : Uhppoted.Lib.Packet := (others => 0);
   begin
      Buffer (1) := 16#17#;
      Buffer (2) := 16#94#;

      return Buffer;
   end Find_Controllers;

end Uhppoted.Lib.Encode;
