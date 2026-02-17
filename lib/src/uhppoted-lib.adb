with Uhppoted.Lib.Encode;
with Uhppoted.Lib.Decode;
with Uhppoted.Lib.UDP;
with Uhppoted.Lib.Types;

package body Uhppoted.Lib is
   use Uhppoted.Lib.Types;

   function Find_Controllers (U : UHPPOTE) return Controller_List is
      Request  : constant Packet := Uhppoted.Lib.Encode.Get_Controller (0);
      Replies  : constant Packet_List := Uhppoted.Lib.UDP.Broadcast (U, Request);
      Response : Controller_List (1 .. Integer (Replies.Length));
      IX       : Positive := 1;
   begin
      for Reply of Replies loop
         declare
            C : Controller;
         begin
            C := Uhppoted.Lib.Decode.Get_Controller (Reply);
            Response (IX) := C;
            IX := IX + 1;
         end;
      end loop;

      return Response;
   end Find_Controllers;

   function Get_Controller (U : UHPPOTE; C : Unsigned_32) return Controller is
      Request : constant Packet := Uhppoted.Lib.Encode.Get_Controller (C);
      Reply   : constant Packet := Uhppoted.Lib.UDP.Send (U, Request);
   begin
      return Uhppoted.Lib.Decode.Get_Controller (Reply);
   end Get_Controller;

end Uhppoted.Lib;
