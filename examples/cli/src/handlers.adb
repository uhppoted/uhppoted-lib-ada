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
      Offset : Time_Offset := Local_Time_Offset(Now);

      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      Seconds    : Day_Duration;
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

end Handlers;
