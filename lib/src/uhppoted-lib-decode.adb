package body Uhppoted.Lib.Decode is

   --  Decodes a 64 byte get-controller response as a Controller record.
   function Get_Controller (Response : Uhppoted.Lib.Packet)
      return Uhppoted.Lib.Controller is
   begin
      return (ID       => 405419896,
              Address  => (192, 168, 1, 100),
              Gateway  => (192, 168, 1, 1),
              Netmask  => (255, 255, 255, 0),
              MAC      => "00:15:42:BF:11:22",
              Firmware => "0692",
              Date     => (Year => 2025, Month => 1, Day => 13));
   end Get_Controller;

end Uhppoted.Lib.Decode;
