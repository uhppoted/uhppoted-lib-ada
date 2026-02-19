with Interfaces;
with Ada.Containers.Vectors;

package Uhppoted.Lib.Integration_Tests.Stub.Messages is

   type Request is array (1 .. 64) of Interfaces.Unsigned_8;
   type Reply is array (1 .. 64) of Interfaces.Unsigned_8;

   package Reply_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Reply);

   subtype Reply_List is Reply_Vectors.Vector;

   function Get (R : Request) return Reply_List;

end Uhppoted.Lib.Integration_Tests.Stub.Messages;
