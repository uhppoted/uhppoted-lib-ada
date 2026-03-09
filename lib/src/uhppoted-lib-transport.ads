with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Streams;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Transport is

   type H is tagged limited private;

   subtype Stream_Packet is Ada.Streams.Stream_Element_Array (1 .. 64);

   function To_Stream is new Ada.Unchecked_Conversion (Source => Uhppoted.Lib.Types.Packet, Target =>  Stream_Packet);
   function To_Packet is new Ada.Unchecked_Conversion (Source => Stream_Packet,  Target => Uhppoted.Lib.Types.Packet);

   Timeout_Error : exception;

private
   type H is new Ada.Finalization.Limited_Controlled with
   record
      Selector  : Selector_Type;
   end record;

   overriding
   procedure Initialize (E : in out H);

   overriding
   procedure Finalize (E : in out H);

end Uhppoted.Lib.Transport;
