with Ada.Containers.Vectors;

package Uhppoted.Lib.Integration_Tests.Stub.Events is

   type Message is array (1 .. 64) of Interfaces.Unsigned_8;

   type Event is record
      Msg   : Message;
      State : Uhppoted.Lib.Controller_State;
      Event : Uhppoted.Lib.Controller_Event;
   end record;

   package Events_Vector is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Event);

   subtype Events_List is Events_Vector.Vector;

   Listener_Event_Message : constant Message := [
      16#17#, 16#20#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#4e#, 16#00#, 16#00#, 16#00#, 16#02#, 16#01#, 16#03#, 16#01#,
      16#a1#, 16#98#, 16#7c#, 16#00#, 16#20#, 16#22#, 16#08#, 16#23#,  16#09#, 16#47#, 16#06#, 16#2c#, 16#00#, 16#01#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#01#, 16#03#, 16#09#, 16#49#, 16#39#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#27#, 16#07#, 16#09#, 16#22#, 16#08#, 16#23#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
   ];

   Listener_Event_V6_62_Message : constant Message := [
      16#19#, 16#20#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,  16#4e#, 16#00#, 16#00#, 16#00#, 16#02#, 16#01#, 16#03#, 16#01#,
      16#a1#, 16#98#, 16#7c#, 16#00#, 16#20#, 16#22#, 16#08#, 16#23#,  16#09#, 16#47#, 16#06#, 16#2c#, 16#00#, 16#01#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#01#, 16#03#, 16#09#, 16#49#, 16#39#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#27#, 16#07#, 16#09#, 16#22#, 16#08#, 16#23#, 16#00#, 16#00#,  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
   ];

   Listener_Event_State : constant Controller_State := (
      System_Date_Time => (Year => 2022, Month => 8, Day => 23, Hour => 9, Minute => 49, Second => 39),
      Doors => [1 => (Open     => False,
                      Button   => False,
                      Unlocked => True),
                2 => (Open     => True,
                      Button   => False,
                      Unlocked => True),
                3 => (Open     => False,
                      Button   => False,
                      Unlocked => True),
                4 => (Open     => False,
                      Button   => True,
                      Unlocked => False)],
      Alarms => (Flags       => 9,
                 Fire        => True,
                 Lock_Forced => False),
      System_Error => 3,
      Special_Info => 39);

   Listener_Event_V6_62_State : constant Controller_State := (
      System_Date_Time => (Year => 2022, Month => 8, Day => 23, Hour => 9, Minute => 49, Second => 39),
      Doors => [1 => (Open     => False,
                      Button   => False,
                      Unlocked => True),
                2 => (Open     => True,
                      Button   => False,
                      Unlocked => True),
                3 => (Open     => False,
                      Button   => False,
                      Unlocked => True),
                4 => (Open     => False,
                      Button   => True,
                      Unlocked => False)],
      Alarms => (Flags       => 9,
                 Fire        => True,
                 Lock_Forced => False),
      System_Error => 3,
      Special_Info => 39);

   Listener_Event_Event : constant Controller_Event := (
      Index          => 78,
      Event          => 2,
      Timestamp      => (Year => 2022, Month => 8, Day => 23, Hour => 9, Minute => 47, Second => 6),
      Door           => 3,
      Direction      => 1,
      Card           => 8165537,
      Access_Granted => true,
      Reason         => 44);

   Listener_Event_V6_62_Event : constant Controller_Event := (
      Index          => 78,
      Event          => 2,
      Timestamp      => (Year => 2022, Month => 8, Day => 23, Hour => 9, Minute => 47, Second => 6),
      Door           => 3,
      Direction      => 1,
      Card           => 8165537,
      Access_Granted => true,
      Reason         => 44);

   Events : constant Events_List := [
      (Listener_Event_Message,       Listener_Event_State,       Listener_Event_Event),
      (Listener_Event_V6_62_Message, Listener_Event_V6_62_State, Listener_Event_V6_62_Event)
   ];

end Uhppoted.Lib.Integration_Tests.Stub.Events;
