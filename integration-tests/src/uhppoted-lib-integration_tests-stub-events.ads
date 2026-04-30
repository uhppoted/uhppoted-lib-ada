with Ada.Containers.Vectors;

package Uhppoted.Lib.Integration_Tests.Stub.Events is

   type Message is array (1 .. 64) of Interfaces.Unsigned_8;

   Listener_Event_Event : constant Message := [
      16#17#, 16#20#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#4e#, 16#00#, 16#00#, 16#00#, 16#02#, 16#01#, 16#03#, 16#01#,
      16#a1#, 16#98#, 16#7c#, 16#00#, 16#20#, 16#22#, 16#08#, 16#23#,  16#09#, 16#47#, 16#06#, 16#2c#, 16#00#, 16#01#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#01#, 16#03#, 16#09#, 16#49#, 16#39#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#27#, 16#07#, 16#09#, 16#22#, 16#08#, 16#23#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
   ];

   Listener_Event_V6_62_Event : constant Message := [
      16#19#, 16#20#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#4e#, 16#00#, 16#00#, 16#00#, 16#02#, 16#01#, 16#03#, 16#01#,
      16#a1#, 16#98#, 16#7c#, 16#00#, 16#20#, 16#22#, 16#08#, 16#23#,  16#09#, 16#47#, 16#06#, 16#2c#, 16#00#, 16#01#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#01#, 16#03#, 16#09#, 16#49#, 16#39#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#27#, 16#07#, 16#09#, 16#22#, 16#08#, 16#23#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
   ];

   type Event is record
      Msg : Message;
   end record;

   package Events_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Event);

   subtype Events_List is Events_Vector.Vector;

   Events : constant Events_List := [
      (Msg => Listener_Event_Event),
      (Msg => Listener_Event_V6_62_Event)
   ];

end Uhppoted.Lib.Integration_Tests.Stub.Events;
