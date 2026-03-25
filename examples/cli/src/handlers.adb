with Interfaces;
with GNAT.Sockets;
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
      Put_Line ("controller:"  & R.ID'Image);
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
      Addr    : constant Inet_Addr_Type := Inet_Addr ("192.168.1.125");
      Netmask : constant Inet_Addr_Type := Inet_Addr ("255.255.255.0");
      Gateway : constant Inet_Addr_Type := Inet_Addr ("192.168.1.1");
      R       : constant Boolean := Set_IPv4 (U, Args.Controller, Addr, Netmask, Gateway, Timeout);
   begin
      Put_Line ("--- set-IPv4");
      Put_Line ("controller:"  & Args.Controller.ID'Image);
      Put_Line ("            " & R'Image);
      Put_Line ("");
   end Set_IPv4;

   --  Executes the get-time command.
   procedure Get_Time (Args : ArgParse.Args) is
      R : constant DateTime := Get_Time (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-time");
      Put_Line ("controller:"  & Args.Controller.ID'Image);
      Put_Line ("            " & Image (R));
      Put_Line ("");
   end Get_Time;

   --  Executes the set-time command.
   procedure Set_Time (Args : ArgParse.Args) is
      Now    : constant Time := Clock;
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
      Put_Line ("controller:"  & Args.Controller.ID'Image);
      Put_Line ("            " & Image (R));
      Put_Line ("");
   end Set_Time;

   --  Executes the get-listener command.
   procedure Get_Listener (Args : ArgParse.Args) is
      R : constant Listener_Record := Get_Listener (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-listener");
      Put_Line ("controller:"  & Args.Controller.ID'Image);
      Put_Line ("  listener: " & Image (R.Listener));
      Put_Line ("  interval:" & R.Interval'Image);
      Put_Line ("");
   end Get_Listener;

   --  Executes the set-listener command.
   procedure Set_Listener (Args : ArgParse.Args) is
      R : constant Boolean := Set_Listener (U, Args.Controller, Args.Listener, Args.Interval, Timeout);
   begin
      Put_Line ("--- set-listener");
      Put_Line ("controller:"  & Args.Controller.ID'Image);
      Put_Line ("        ok: " & R'Image);
      Put_Line ("");
   end Set_Listener;

   --  Executes the get-status command.
   procedure Get_Status (Args : ArgParse.Args) is
      R : constant Controller_Status := Get_Status (U, Args.Controller, Timeout);
   begin
      Put_Line ("--- get-status");
      Put_Line ("controller:"  & Args.Controller.ID'Image);
      Put_Line ("  system date/time:        " & Image (R.System_Date_Time));
      Put_Line ("  door 1 - open:           " & R.Doors (1).Open'Image);
      Put_Line ("           button pressed: " & R.Doors (1).Button'Image);
      Put_Line ("           unlocked:       " & R.Doors (1).Unlocked'Image);
      Put_Line ("  door 2 - open:           " & R.Doors (2).Open'Image);
      Put_Line ("           button pressed: " & R.Doors (2).Button'Image);
      Put_Line ("           unlocked:       " & R.Doors (2).Unlocked'Image);
      Put_Line ("  door 3 - open:           " & R.Doors (3).Open'Image);
      Put_Line ("           button pressed: " & R.Doors (3).Button'Image);
      Put_Line ("           unlocked:       " & R.Doors (3).Unlocked'Image);
      Put_Line ("  door 4 - open:           " & R.Doors (4).Open'Image);
      Put_Line ("           button pressed: " & R.Doors (4).Button'Image);
      Put_Line ("           unlocked:       " & R.Doors (4).Unlocked'Image);
      Put_Line ("  alarms - flags:         " & R.Alarms.Flags'Image);
      Put_Line ("           fire:           " & R.Alarms.Fire'Image);
      Put_Line ("           lock forced:    " & R.Alarms.Lock_Forced'Image);
      Put_Line ("  system error:           " & R.System_Error'Image);
      Put_Line ("  special info:           " & R.Special_Info'Image);
      Put_Line ("  event - index:          " & R.Event.Index'Image);
      Put_Line ("          type:           " & R.Event.Event'Image);
      Put_Line ("          timestamp:       " & Image (R.Event.Timestamp));
      Put_Line ("          door:           " & R.Event.Door'Image);
      Put_Line ("          direction:      " & R.Event.Direction'Image);
      Put_Line ("          card:           " & R.Event.Card'Image);
      Put_Line ("          access granted:  " & R.Event.Access_Granted'Image);
      Put_Line ("          reason:         " & R.Event.Reason'Image);

      Put_Line ("");
   end Get_Status;

   --  Executes the get-door command.
   procedure Get_Door (Args : ArgParse.Args) is
      R : constant Door_Record := Get_Door (U, Args.Controller, Args.Door, Timeout);
   begin
      Put_Line ("--- get-door");
      Put_Line ("controller:" & Args.Controller.ID'Image);
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
      Put_Line ("      mode: " & R.Mode'Image);
      Put_Line ("     delay:" & R.OpenDelay'Image);
      Put_Line ("");
   end Set_Door;

end Handlers;
