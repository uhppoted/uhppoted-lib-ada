with AUnit.Assertions;
with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Containers.Vectors;
with GNAT.Sockets;

with Uhppoted.Lib.Integration_Tests.Stub.Events;

package body Uhppoted.Lib.Integration_Tests.Listen is
   use AUnit.Assertions;
   use Ada.Streams;
   use type Ada.Containers.Count_Type;
   use Uhppoted.Lib.Integration_Tests.Stub.Events;

   U : constant UHPPOTE := (
      Bind_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 0),

      Broadcast_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("255.255.255.255"),
         Port => 60000),

      Listen_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 60001),

      Debug => False);

   subtype Stream_Packet is Ada.Streams.Stream_Element_Array (1 .. 64);

   function To_Stream is new Ada.Unchecked_Conversion (Source => Uhppoted.Lib.Integration_Tests.Stub.Events.Message,
                                                       Target => Stream_Packet);

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("listen tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Listen'Access,    "listen");
   end Register_Tests;

   overriding procedure Set_Up_Case (T : in out Integration_Test) is
   begin
      null;
   end Set_Up_Case;

   overriding procedure Tear_Down_Case (T : in out Integration_Test) is
   begin
      null;
   end Tear_Down_Case;

   overriding procedure Set_Up (T : in out Integration_Test) is
   begin
      null;
   end Set_Up;

   overriding procedure Tear_Down (T : in out Integration_Test) is
   begin
      null;
   end Tear_Down;

   --  listen event handler
   type ListenEvent is record
      State : Uhppoted.Lib.Controller_State;
      Event : Uhppoted.Lib.Controller_Event;
   end record;

   package ListenEvents_Vector is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => ListenEvent);

   protected type EventsList is
      procedure Append (E : ListenEvent);
      function Length return Ada.Containers.Count_Type;
      function Get (I : Natural) return ListenEvent;
   private
      List : ListenEvents_Vector.Vector;
   end EventsList;

   protected body EventsList is
      procedure Append (E : ListenEvent) is
      begin
         List.Append (E);
      end Append;

      function Length return Ada.Containers.Count_Type is
      begin
         return List.Length;
      end Length;

      function Get (I : Natural) return ListenEvent is
      begin
         return List (I);
      end Get;
   end EventsList;

   type EventsList_Access is access all EventsList;

   type Listener is new Uhppoted.Lib.Event_Handler with record
      Q : not null EventsList_Access := new EventsList;
   end record;

   overriding procedure On_Event (Self       : Listener;
                                  Controller : Unsigned_32;
                                  State      : Uhppoted.Lib.Controller_State;
                                  Event      : Uhppoted.Lib.Controller_Event) is
      E : constant ListenEvent := (State, Event);
   begin
      Self.Q.Append (E);
   end On_Event;

   procedure Test_Listen (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      L : Listener;
      S : Signal;

      task E is
         entry Done;
      end E;

      task body E is
      begin
         Uhppoted.Lib.Listen (U, L, S);
         accept Done;
      end E;

      task D is
         entry Done;
      end D;

      task body D is
         Socket   : GNAT.Sockets.Socket_Type;
         DestAddr : constant Sock_Addr_Type := (Family => GNAT.Sockets.Family_Inet,
                                                Addr => Loopback_Inet_Addr,
                                                Port => 60001);
      begin
         delay 0.5;

         Create_Socket (Socket, Family_Inet, Socket_Datagram);
         for V of Events loop
            declare
               Offset    : Stream_Element_Offset;
            begin
               Send_Socket (Socket, To_Stream (V.Msg), Offset, DestAddr);
               delay 0.25;
            end;
         end loop;

         Trigger (S);
         Close_Socket (Socket);
         accept Done;
      end D;

      N : constant Ada.Containers.Count_Type := Events.Length;
   begin
      E.Done;
      D.Done;

      Assert (L.Q.Length = N, "incorrect number of events (" & L.Q.Length'Image & ")");

      for I in 1 .. N loop
         declare
            IX : constant Natural := Natural (I);
            U  : constant ListenEvent := L.Q.Get (IX);
            V  : constant Event := Events (IX);
         begin
            Assert (U.State = V.State, "incorrect event state: " & U.State'Image);
            Assert (U.Event = V.event, "incorrect event event: " & U.Event'Image);
         end;
      end loop;

   end Test_Listen;

end Uhppoted.Lib.Integration_Tests.Listen;
