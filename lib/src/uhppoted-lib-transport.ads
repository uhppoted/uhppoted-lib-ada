with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Streams;
with Uhppoted.Lib.Types;

--  Internal definitions for the UDP and TCP transport implementations.
--
package Uhppoted.Lib.Transport is

   --  Controlled Type for a socket selector.
   type H is tagged limited private;

   --  64 byte packet type
   subtype Stream_Packet is Ada.Streams.Stream_Element_Array (1 .. 64);

   --  Converts a Packet to a Stream_Packet.
   function To_Stream is new Ada.Unchecked_Conversion (Source => Uhppoted.Lib.Types.Packet, Target => Stream_Packet);

   --  Converts a Stream_Packet to a Packet.
   function To_Packet is new Ada.Unchecked_Conversion (Source => Stream_Packet, Target => Uhppoted.Lib.Types.Packet);

   --  Interface type for an event handler.
   type Event_Dispatcher is interface;

   --  Event handler for an event received via the UDP 'listen' implementation.
   --
   --  @param  Self  External handler for the received event.
   --  @param  Msg   Event packet.
   procedure On_Event (Self : Event_Dispatcher; Msg : Uhppoted.Lib.Types.Packet) is abstract;

private
   --  Controlled Type for a socket selector.
   --
   --  @field  Selector  GNAT.Sockets.Selector for socket.
   type H is new Ada.Finalization.Limited_Controlled with record
      Selector : Selector_Type;
   end record;

   --  Creates the wrapped selector handle.
   --
   --  @param  E  Controlled.Initialize implementation for socket selector.
   overriding procedure Initialize (E : in out H);

   --  Closes the wrapped selector handle.
   --
   --  @param  E  Controlled.Finalize implementation for socket selector.
   overriding procedure Finalize (E : in out H);

   --  Prints out a hex dump of a 64 byte packet.
   --
   --  @param  Msg  Message description.
   --  @param  P    Packet to print out.
   procedure Dump (Msg : String; P : Uhppoted.Lib.Types.Packet);

end Uhppoted.Lib.Transport;
