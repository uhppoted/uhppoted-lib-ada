with Uhppoted.Lib.Requests;

package body Uhppoted.Lib.Encode is
   use Uhppoted.Lib.Requests;

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (Controller : Unsigned_32) return Packet is
      Request : GetControllerRequest;
      Buffer  : Packet with Address => Request'Address;
   begin
      Request.Controller := Controller;

      return Buffer;
   end Get_Controller;

end Uhppoted.Lib.Encode;
