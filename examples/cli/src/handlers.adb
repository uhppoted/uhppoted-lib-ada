with Interfaces;
with GNAT.Sockets;
with GNAT.Ctrl_C;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Uhppoted.Lib;

package body Handlers is
   use Interfaces;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Ada.Calendar;
   use Ada.Calendar.Formatting;
   use Ada.Calendar.Time_Zones;
   use GNAT.Sockets;
   use GNAT.Ctrl_C;

   use Uhppoted.Lib;

   U : constant UHPPOTE := (
      Bind_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Any_Inet_Addr,
         Port => 0),

      Broadcast_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("192.168.1.255"),
         Port => 60000),

      Listen_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Any_Inet_Addr,
         Port => 60001),

      Debug => True);

   Timeout : constant Duration := 2.5;

   --  Executes the find-controllers command.
   procedure Find_Controllers (Args : ArgParse.Args) is
      pragma Unreferenced (Args);

      Controllers : constant Controller_Record_List := Find_Controllers (U, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- find-controllers");

      if Controllers'Length = 0 then
         Ada.Text_IO.Put_Line ("No controllers found.");
      else
         for C of Controllers loop
            Put_Line ("controller:" & C.ID'Image);
            Put_Line ("            " & Image (C.Address));
            Put_Line ("            " & Image (C.Netmask));
            Put_Line ("            " & Image (C.Gateway));
            Put_Line ("            " & Image (C.MAC));
            Put_Line ("            " & To_String (C.Firmware));
            Put_Line ("            " & Image (C.Date));
            Put_Line ("");
         end loop;
      end if;

      Put_Line ("");
   end Find_Controllers;

   --  Executes the get-controller command.
   procedure Get_Controller (Args : ArgParse.Args) is
      R : constant Controller_Record := Get_Controller (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-controller");
      Put_Line ("controller:" & R.ID'Image);
      Put_Line ("            " & Image (R.Address));
      Put_Line ("            " & Image (R.Netmask));
      Put_Line ("            " & Image (R.Gateway));
      Put_Line ("            " & Image (R.MAC));
      Put_Line ("            " & To_String (R.Firmware));
      Put_Line ("            " & Image (R.Date));
      Put_Line ("");
   end Get_Controller;

   --  Executes the set-IPv4 command.
   procedure Set_IPv4 (Args : ArgParse.Args) is
      R : constant Boolean := Set_IPv4 (U, Args.Controller, Args.Address, Args.Netmask, Args.Gateway, Timeout);
   begin
      Put_Line ("--- set-IPv4");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("            " & R'Image);
      Put_Line ("");
   end Set_IPv4;

   --  Executes the get-time command.
   procedure Get_Time (Args : ArgParse.Args) is
      R : constant DateTime := Get_Time (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-time");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("            " & Image (R));
      Put_Line ("");
   end Get_Time;

   --  Executes the set-time command.
   procedure Set_Time (Args : ArgParse.Args) is
      Now    : constant Time        := Clock;
      Offset : constant Time_Offset := Local_Time_Offset (Now);

      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Second_Duration;

      DT : DateTime;
      R  : DateTime;
   begin
      Split (Date       => Now,
             Year       => Year,
             Month      => Month,
             Day        => Day,
             Hour       => Hour,
             Minute     => Minute,
             Second     => Second,
             Sub_Second => Sub_Second,
             Time_Zone  => Offset);

      DT := (Year   => Unsigned_16 (Year),
             Month  => Unsigned_8 (Month),
             Day    => Unsigned_8 (Day),
             Hour   => Unsigned_8 (Hour),
             Minute => Unsigned_8 (Minute),
             Second => Unsigned_8 (Second));

      R := Uhppoted.Lib.Set_Time (U, Args.Controller, DT, Timeout);

      Put_Line ("--- set-time");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("            " & Image (R));
      Put_Line ("");
   end Set_Time;

   --  Executes the get-listener command.
   procedure Get_Listener (Args : ArgParse.Args) is
      R : constant Listener_Record := Get_Listener (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-listener");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("  listener: " & Image (R.Listener));
      Put_Line ("  interval:" & R.Interval'Image);
      Put_Line ("");
   end Get_Listener;

   --  Executes the set-listener command.
   procedure Set_Listener (Args : ArgParse.Args) is
      R : constant Boolean := Set_Listener (U, Args.Controller, Args.Listener, Args.Interval, Timeout);
   begin
      Put_Line ("--- set-listener");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("        ok: " & R'Image);
      Put_Line ("");
   end Set_Listener;

   --  Executes the get-status command.
   procedure Get_Status (Args : ArgParse.Args) is
      R : constant Controller_Status := Get_Status (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-status");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("");
      Put_Line ("   system date/time:        " & Image (R.State.System_Date_Time));
      Put_Line ("     door 1 open:           " & R.State.Doors (1).Open'Image);
      Put_Line ("            button pressed: " & R.State.Doors (1).Button'Image);
      Put_Line ("            unlocked:       " & R.State.Doors (1).Unlocked'Image);
      Put_Line ("     door 2 open:           " & R.State.Doors (2).Open'Image);
      Put_Line ("            button pressed: " & R.State.Doors (2).Button'Image);
      Put_Line ("            unlocked:       " & R.State.Doors (2).Unlocked'Image);
      Put_Line ("     door 3 open:           " & R.State.Doors (3).Open'Image);
      Put_Line ("            button pressed: " & R.State.Doors (3).Button'Image);
      Put_Line ("            unlocked:       " & R.State.Doors (3).Unlocked'Image);
      Put_Line ("     door 4 open:           " & R.State.Doors (4).Open'Image);
      Put_Line ("            button pressed: " & R.State.Doors (4).Button'Image);
      Put_Line ("            unlocked:       " & R.State.Doors (4).Unlocked'Image);
      Put_Line ("      alarm flags:         " & R.State.Alarms.Flags'Image);
      Put_Line ("            fire:           " & R.State.Alarms.Fire'Image);
      Put_Line ("            lock forced:    " & R.State.Alarms.Lock_Forced'Image);
      Put_Line ("   system error:           " & R.State.System_Error'Image);
      Put_Line ("   special info:           " & R.State.Special_Info'Image);
      Put_Line ("");
      Put_Line ("     event index:          " & R.Event.Index'Image);
      Put_Line ("           type:            " & R.Event.Event'Image);
      Put_Line ("           timestamp:       " & Image (R.Event.Timestamp));
      Put_Line ("           door:           " & R.Event.Door'Image);
      Put_Line ("           direction:        " & Image (R.Event.Direction));
      Put_Line ("           card:           " & R.Event.Card'Image);
      Put_Line ("           access granted:  " & R.Event.Access_Granted'Image);
      Put_Line ("           reason:          " & R.Event.Reason'Image);

      Put_Line ("");
   end Get_Status;

   --  Executes the get-door command.
   procedure Get_Door (Args : ArgParse.Args) is
      R : constant Door_Record := Get_Door (U, Args.Controller, Args.Door, Timeout);
   begin
      Put_Line ("--- get-door");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("      door:" & Args.Door'Image);
      Put_Line ("      mode: " & R.Mode'Image);
      Put_Line ("     delay:" & R.OpenDelay'Image);
      Put_Line ("");
   end Get_Door;

   --  Executes the set-door command.
   procedure Set_Door (Args : ArgParse.Args) is
      R : constant Door_Record := Set_Door (U, Args.Controller, Args.Door, Args.Mode, Args.OpenDelay, Timeout);
   begin
      Put_Line ("--- set-door");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("      door:" & Args.Door'Image);
      Put_Line ("      mode: " & R.Mode'Image);
      Put_Line ("     delay:" & R.OpenDelay'Image);
      Put_Line ("");
   end Set_Door;

   --  Executes the set-door-passcodes command.
   procedure Set_Door_Passcodes (Args : ArgParse.Args) is
      R : constant Boolean := Set_Door_Passcodes (U, Args.Controller, Args.Door, Args.Passcodes, Timeout);
   begin
      Put_Line ("--- set-door-passcodes");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("      door:" & Args.Door'Image);
      Put_Line ("        ok: " & R'Image);
      Put_Line ("");
   end Set_Door_Passcodes;

   --  Executes the open-door command.
   procedure Open_Door (Args : ArgParse.Args) is
      R : constant Boolean := Open_Door (U, Args.Controller, Args.Door, Timeout);
   begin
      Put_Line ("--- open-door");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("      door:" & Args.Door'Image);
      Put_Line ("        ok: " & R'Image);
      Put_Line ("");
   end Open_Door;

   --  Executes the get-cards command.
   procedure Get_Cards (Args : ArgParse.Args) is
      R : constant Unsigned_32 := Get_Cards (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-cards");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("     cards:" & R'Image);
      Put_Line ("");
   end Get_Cards;

   --  Executes the get-card command.
   procedure Get_Card (Args : ArgParse.Args) is
      R : constant Card_Record := Get_Card (U, Args.Controller, Args.Card.Card, Timeout);
   begin
      Put_Line ("--- get-card");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("       card:" & R.Card'Image);
      Put_Line (" start date: " & Image (R.Start_Date));
      Put_Line ("   end date: " & Image (R.End_Date));
      Put_Line ("     door 1:" & R.Door_1'Image);
      Put_Line ("     door 2:" & R.Door_2'Image);
      Put_Line ("     door 3:" & R.Door_3'Image);
      Put_Line ("     door 4:" & R.Door_4'Image);
      Put_Line ("        PIN:" & R.PIN'Image);
      Put_Line ("");
   end Get_Card;

   --  Executes the get-card-at-index command.
   procedure Get_Card_At_Index (Args : ArgParse.Args) is
      R : constant Card_Record := Get_Card_At_Index (U, Args.Controller, Args.Card_Index, Timeout);
   begin
      Put_Line ("--- get-card-at-index");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("      index:" & Args.Card_Index'Image);
      Put_Line ("       card:" & R.Card'Image);
      Put_Line (" start date: " & Image (R.Start_Date));
      Put_Line ("   end date: " & Image (R.End_Date));
      Put_Line ("     door 1:" & R.Door_1'Image);
      Put_Line ("     door 2:" & R.Door_2'Image);
      Put_Line ("     door 3:" & R.Door_3'Image);
      Put_Line ("     door 4:" & R.Door_4'Image);
      Put_Line ("        PIN:" & R.PIN'Image);
      Put_Line ("");
   end Get_Card_At_Index;

   --  Executes the put-card command.
   procedure Put_Card (Args : ArgParse.Args) is
      R : constant Boolean := Put_Card (U, Args.Controller, Args.Card, Timeout);
   begin
      Put_Line ("--- put-card");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("       card:" & Args.Card.Card'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Put_Card;

   --  Executes the delete-card command.
   procedure Delete_Card (Args : ArgParse.Args) is
      R : constant Boolean := Delete_Card (U, Args.Controller, Args.Card.Card, Timeout);
   begin
      Put_Line ("--- delete-card");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("       card:" & Args.Card.Card'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Delete_Card;

   --  Executes the delete-all-cards command.
   procedure Delete_All_Cards (Args : ArgParse.Args) is
      R : constant Boolean := Delete_All_Cards (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- delete-all-cards");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Delete_All_Cards;

   --  Executes the get-event command.
   procedure Get_Event (Args : ArgParse.Args) is
      R : constant Controller_Event := Get_Event (U, Args.Controller, Args.Event_Index, Timeout);
   begin
      Put_Line ("--- get-event");
      Put_Line ("controller:" & Args.Controller.ID'Image);
      Put_Line ("  index:          " & R.Index'Image);
      Put_Line ("  event:           " & R.Event'Image);
      Put_Line ("  timestamp:       " & Image (R.Timestamp));
      Put_Line ("  door:           " & R.Door'Image);
      Put_Line ("  direction:       " & Image (R.Direction));
      Put_Line ("  card:           " & R.Card'Image);
      Put_Line ("  access granted:  " & R.Access_Granted'Image);
      Put_Line ("  reason:          " & R.Reason'Image);

      Put_Line ("");
   end Get_Event;

   --  Executes the get-event-index command.
   procedure Get_Event_Index (Args : ArgParse.Args) is
      R : constant Unsigned_32 := Get_Event_Index (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-event-index");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("      index:" & R'Image);
      Put_Line ("");
   end Get_Event_Index;

   --  Executes the set-event-index command.
   procedure Set_Event_Index (Args : ArgParse.Args) is
      R : constant Boolean := Set_Event_Index (U, Args.Controller, Args.Event_Index, Timeout);
   begin
      Put_Line ("--- set-event-index");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("      index:" & Args.Event_Index'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Set_Event_Index;

   --  Executes the record-special-events command.
   procedure Record_Special_Events (Args : ArgParse.Args) is
      R : constant Boolean := Record_Special_Events (U, Args.Controller, Args.Enabled, Timeout);
   begin
      Put_Line ("--- record-special-events");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("    enabled: " & Args.Enabled'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Record_Special_Events;

   --  Executes the get-time-profile command.
   procedure Get_Time_Profile (Args : ArgParse.Args) is
      R : constant Time_Profile := Get_Time_Profile (U, Args.Controller, Args.Profile_ID, Timeout);
   begin
      Put_Line ("--- get-time-profile ");
      Put_Line ("         controller:" & Args.Controller.ID'Image);
      Put_Line ("            profile:" & Args.Profile_ID'Image);
      Put_Line ("         start-date: " & Image (R.Start_Date));
      Put_Line ("           end-date: " & Image (R.End_Date));
      Put_Line ("             monday: " & R.Weekdays.Monday'Image);
      Put_Line ("            tuesday: " & R.Weekdays.Tuesday'Image);
      Put_Line ("          wednesday: " & R.Weekdays.Wednesday'Image);
      Put_Line ("           thursday: " & R.Weekdays.Thursday'Image);
      Put_Line ("             friday: " & R.Weekdays.Friday'Image);
      Put_Line ("           saturday: " & R.Weekdays.Saturday'Image);
      Put_Line ("             sunday: " & R.Weekdays.Sunday'Image);
      Put_Line ("    segment 1 start: " & Image (R.Segments (1).Start_Time));
      Put_Line ("                end: " & Image (R.Segments (1).End_Time));
      Put_Line ("    segment 2 start: " & Image (R.Segments (2).Start_Time));
      Put_Line ("                end: " & Image (R.Segments (2).End_Time));
      Put_Line ("    segment 3 start: " & Image (R.Segments (3).Start_Time));
      Put_Line ("                end: " & Image (R.Segments (3).End_Time));
      Put_Line ("     linked profile:" & R.Linked_Profile'Image);
      Put_Line ("");
   end Get_Time_Profile;

   --  Executes the set-time-profile command.
   procedure Set_Time_Profile (Args : ArgParse.Args) is
      R : constant Boolean := Set_Time_Profile (U, Args.Controller, Args.Profile_ID, Args.Profile, Timeout);
   begin
      Put_Line ("--- set-time-profile ");
      Put_Line ("         controller:" & Args.Controller.ID'Image);
      Put_Line ("            profile:" & Args.Profile_ID'Image);
      Put_Line ("         start-date: " & Image (Args.Profile.Start_Date));
      Put_Line ("           end-date: " & Image (Args.Profile.End_Date));
      Put_Line ("             monday: " & Args.Profile.Weekdays.Monday'Image);
      Put_Line ("            tuesday: " & Args.Profile.Weekdays.Tuesday'Image);
      Put_Line ("          wednesday: " & Args.Profile.Weekdays.Wednesday'Image);
      Put_Line ("           thursday: " & Args.Profile.Weekdays.Thursday'Image);
      Put_Line ("             friday: " & Args.Profile.Weekdays.Friday'Image);
      Put_Line ("           saturday: " & Args.Profile.Weekdays.Saturday'Image);
      Put_Line ("             sunday: " & Args.Profile.Weekdays.Sunday'Image);
      Put_Line ("    segment 1 start: " & Image (Args.Profile.Segments (1).Start_Time));
      Put_Line ("                end: " & Image (Args.Profile.Segments (1).End_Time));
      Put_Line ("    segment 2 start: " & Image (Args.Profile.Segments (2).Start_Time));
      Put_Line ("                end: " & Image (Args.Profile.Segments (2).End_Time));
      Put_Line ("    segment 3 start: " & Image (Args.Profile.Segments (3).Start_Time));
      Put_Line ("                end: " & Image (Args.Profile.Segments (3).End_Time));
      Put_Line ("     linked profile:" & Args.Profile.Linked_Profile'Image);
      Put_Line ("");
      Put_Line ("                 ok: " & R'Image);
   end Set_Time_Profile;

   --  Executes the clear-time-profiles command.
   procedure Clear_Time_Profiles (Args : ArgParse.Args) is
      R : constant Boolean := Clear_Time_Profiles (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- clear-time-profiles ");
      Put_Line ("         controller:" & Args.Controller.ID'Image);
      Put_Line ("                 ok: " & R'Image);
   end Clear_Time_Profiles;

   --  Executes the add-task command.
   procedure Add_Task (Args : ArgParse.Args) is
      R : constant Boolean := Add_Task (U, Args.Controller, Args.TaskT, Timeout);
   begin
      Put_Line ("--- add-task ");
      Put_Line ("         controller:" & Args.Controller.ID'Image);
      Put_Line ("               task: " & Args.TaskT.Task_ID'Image);
      Put_Line ("               door:" & Args.TaskT.Door'Image);
      Put_Line ("         start-date: " & Image (Args.TaskT.Start_Date));
      Put_Line ("           end-date: " & Image (Args.TaskT.End_Date));
      Put_Line ("             monday: " & Args.TaskT.Weekdays.Monday'Image);
      Put_Line ("            tuesday: " & Args.TaskT.Weekdays.Tuesday'Image);
      Put_Line ("          wednesday: " & Args.TaskT.Weekdays.Wednesday'Image);
      Put_Line ("           thursday: " & Args.TaskT.Weekdays.Thursday'Image);
      Put_Line ("             friday: " & Args.TaskT.Weekdays.Friday'Image);
      Put_Line ("           saturday: " & Args.TaskT.Weekdays.Saturday'Image);
      Put_Line ("             sunday: " & Args.TaskT.Weekdays.Sunday'Image);
      Put_Line ("         start time: " & Image (Args.TaskT.Start_Time));
      Put_Line ("         more cards:" & Args.TaskT.More_Cards'Image);
      Put_Line ("");
      Put_Line ("                 ok: " & R'Image);
   end Add_Task;

   --  Executes the refresh-tasklist command.
   procedure Refresh_Task_List (Args : ArgParse.Args) is
      R : constant Boolean := Refresh_Task_List (U, Args.Controller,  Timeout);
   begin
      Put_Line ("--- refresh-tasklist");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Refresh_Task_List;

   --  Executes the clear-tasklist command.
   procedure Clear_Task_List (Args : ArgParse.Args) is
      R : constant Boolean := Clear_Task_List (U, Args.Controller,  Timeout);
   begin
      Put_Line ("--- clear-tasklist");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Clear_Task_List;

   --  Executes the set-pc-control command.
   procedure Set_PC_Control (Args : ArgParse.Args) is
      R : constant Boolean := Set_PC_Control (U, Args.Controller, Args.Enable, Timeout);
   begin
      Put_Line ("--- set-pc-control");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("    enabled: " & Args.Enable'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Set_PC_Control;

   --  Executes the set-interlock command.
   procedure Set_Interlock (Args : ArgParse.Args) is
      R : constant Boolean := Set_Interlock (U, Args.Controller, Args.Interlock, Timeout);
   begin
      Put_Line ("--- set-interlock");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("  interlock: " & Args.Interlock'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Set_Interlock;

   --  Executes the activate-keypads command.
   procedure Activate_Keypads (Args : ArgParse.Args) is
      R : constant Boolean := Activate_Keypads (U, Args.Controller, Args.Keypads, Timeout);
   begin
      Put_Line ("--- activate-keypads");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("  keypads 1:" & Args.Keypads (1)'Image);
      Put_Line ("          2:" & Args.Keypads (2)'Image);
      Put_Line ("          3:" & Args.Keypads (3)'Image);
      Put_Line ("          4:" & Args.Keypads (4)'Image);
      Put_Line ("");
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Activate_Keypads;

   --  Executes the get-antipassback command.
   procedure Get_Antipassback (Args : ArgParse.Args) is
      R : constant Antipassback := Get_Antipassback (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-antipassback");
      Put_Line (" controller:    " & Args.Controller.ID'Image);
      Put_Line ("  anti-passback: " & R'Image);
      Put_Line ("");
   end Get_Antipassback;

   --  Executes the restore-default-parameters command.
   procedure Restore_Default_Parameters (Args : ArgParse.Args) is
      R : constant Boolean := Restore_Default_Parameters (U, Args.Controller,  Timeout);
   begin
      Put_Line ("--- restore-default-parameters");
      Put_Line (" controller:" & Args.Controller.ID'Image);
      Put_Line ("         ok: " & R'Image);
      Put_Line ("");
   end Restore_Default_Parameters;

   --  Executes the listen command.
   S : Signal;

   type Listener is new Uhppoted.Lib.Event_Handler with record
      null;
   end record;

   overriding procedure On_Event (Self       : Listener;
                                  Controller : Unsigned_32;
                                  State      : Uhppoted.Lib.Controller_State;
                                  Event      : Uhppoted.Lib.Controller_Event) is
   begin
      Put_Line ("--- listen-event");
      Put_Line (" controller:" & Controller'Image);
      Put_Line ("");
      Put_Line ("   system date/time:        " & Image (State.System_Date_Time));
      Put_Line ("     door 1 open:           " & State.Doors (1).Open'Image);
      Put_Line ("            button pressed: " & State.Doors (1).Button'Image);
      Put_Line ("            unlocked:       " & State.Doors (1).Unlocked'Image);
      Put_Line ("     door 2 open:           " & State.Doors (2).Open'Image);
      Put_Line ("            button pressed: " & State.Doors (2).Button'Image);
      Put_Line ("            unlocked:       " & State.Doors (2).Unlocked'Image);
      Put_Line ("     door 3 open:           " & State.Doors (3).Open'Image);
      Put_Line ("            button pressed: " & State.Doors (3).Button'Image);
      Put_Line ("            unlocked:       " & State.Doors (3).Unlocked'Image);
      Put_Line ("     door 4 open:           " & State.Doors (4).Open'Image);
      Put_Line ("            button pressed: " & State.Doors (4).Button'Image);
      Put_Line ("            unlocked:       " & State.Doors (4).Unlocked'Image);
      Put_Line ("      alarm flags:         " & State.Alarms.Flags'Image);
      Put_Line ("            fire:           " & State.Alarms.Fire'Image);
      Put_Line ("            lock forced:    " & State.Alarms.Lock_Forced'Image);
      Put_Line ("   system error:           " & State.System_Error'Image);
      Put_Line ("   special info:           " & State.Special_Info'Image);
      Put_Line ("");
      Put_Line ("     event index:          " & Event.Index'Image);
      Put_Line ("           type:           " & Event.Event'Image);
      Put_Line ("           timestamp:       " & Image (Event.Timestamp));
      Put_Line ("           door:           " & Event.Door'Image);
      Put_Line ("           direction:       " & Event.Direction'Image);
      Put_Line ("           card:           " & Event.Card'Image);
      Put_Line ("           access granted:  " & Event.Access_Granted'Image);
      Put_Line ("           reason:          " & Event.Reason'Image);
      Put_Line ("");
   end On_Event;

   procedure SIGINT is
   begin
      Trigger (S);
   end SIGINT;

   procedure Listen (Args : ArgParse.Args) is
      pragma Unreferenced (Args);

      L : Listener;

      task T;

      task body T is
      begin
         Put_Line ("--- listening");
         Listen (U, L, S);
      end T;

   begin
      Install_Handler (Handler => SIGINT'Access);
   end Listen;

end Handlers;
