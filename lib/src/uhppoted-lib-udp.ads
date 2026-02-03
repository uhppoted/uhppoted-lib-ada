package Uhppoted.Lib.UDP is

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   function Send (Request : Uhppoted.Lib.Packet) return Uhppoted.Lib.Packet;

end Uhppoted.Lib.UDP;
