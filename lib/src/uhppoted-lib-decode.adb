with Ada.Unchecked_Conversion;
with Ada.Streams;
with System;

package body Uhppoted.Lib.Decode is
   type GetControllerResponse is record
      ID : Unsigned_32;
      Address : IPv4;
      Netmask : IPv4;
      Gateway : IPv4;
      MAC : MAC_Address;
      Reserved : Ada.Streams.Stream_Element_Array (1 .. 38);
   end record;

   for  GetControllerResponse use record
      ID at 0 range  32 ..  63;
      Address at 0 range 64 .. 95;
      Netmask at 0 range 96 .. 127;
      Gateway at 0 range 128 .. 159;
      MAC at 0 range 160 .. 207;
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
              Firmware => "0692",
              Date     => (Year => 2025, Month => 1, Day => 13));
   end Get_Controller;

end Uhppoted.Lib.Decode;
