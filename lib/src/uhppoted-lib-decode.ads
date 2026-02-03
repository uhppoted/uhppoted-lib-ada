package Uhppoted.Lib.Decode is

   --  Decodes a 64 byte get-controller response as a Controller record.
   function Get_Controller (Response : Uhppoted.Lib.Packet)
      return Uhppoted.Lib.Controller;

end Uhppoted.Lib.Decode;
