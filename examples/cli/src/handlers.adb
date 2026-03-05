with GNAT.Sockets;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Uhppoted.Lib;

package body Handlers is
   use Ada.Strings.Unbounded;
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
            Ada.Text_IO.Put_Line ("controller:" & C.ID'Image);
            Ada.Text_IO.Put_Line ("            " & Image (C.Address));
            Ada.Text_IO.Put_Line ("            " & Image (C.Netmask));
            Ada.Text_IO.Put_Line ("            " & Image (C.Gateway));
            Ada.Text_IO.Put_Line ("            " & Image (C.MAC));
            Ada.Text_IO.Put_Line ("            " & To_String (C.Firmware));
            Ada.Text_IO.Put_Line ("            " & Image (C.Date));
            Ada.Text_IO.Put_Line ("");
         end loop;
      end if;

      Ada.Text_IO.Put_Line ("");
   end Find_Controllers;

   --  Executes the get-controller command.
   procedure Get_Controller (Args : ArgParse.Args) is
      R : constant Controller_Record := Get_Controller (U, Args.Controller, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- get-controller");
      Ada.Text_IO.Put_Line ("controller:"  & R.ID'Image);
      Ada.Text_IO.Put_Line ("            " & Image (R.Address));
      Ada.Text_IO.Put_Line ("            " & Image (R.Netmask));
      Ada.Text_IO.Put_Line ("            " & Image (R.Gateway));
      Ada.Text_IO.Put_Line ("            " & Image (R.MAC));
      Ada.Text_IO.Put_Line ("            " & To_String (R.Firmware));
      Ada.Text_IO.Put_Line ("            " & Image (R.Date));
      Ada.Text_IO.Put_Line ("");
   end Get_Controller;

   --  Executes the set-IPv4 command.
   procedure Set_IPv4 (Args : ArgParse.Args) is
      Addr    : constant Inet_Addr_Type := Inet_Addr ("192.168.1.125");
      Netmask : constant Inet_Addr_Type := Inet_Addr ("255.255.255.0");
      Gateway : constant Inet_Addr_Type := Inet_Addr ("192.168.1.1");
      R       : constant Boolean := Set_IPv4 (U, Args.Controller, Addr, Netmask, Gateway, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- set-IPv4");
      Ada.Text_IO.Put_Line ("controller:"  & Args.Controller.ID'Image);
      Ada.Text_IO.Put_Line ("            " & R'Image);
      Ada.Text_IO.Put_Line ("");
   end Set_IPv4;

   --  Executes the get-time command.
   procedure Get_Time (Args : ArgParse.Args) is
      R : constant DateTime := Get_Time (U, Args.Controller, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- get-time");
      Ada.Text_IO.Put_Line ("controller:"  & Args.Controller.ID'Image);
      Ada.Text_IO.Put_Line ("            " & Image (R));
      Ada.Text_IO.Put_Line ("");
   end Get_Time;

   --  Executes the set-time command.
   procedure Set_Time (Args : ArgParse.Args) is
      DT : constant DateTime := (2026, 3, 4, 12, 5, 9);
      R  : constant DateTime := Uhppoted.Lib.Set_Time (U, Args.Controller, DT, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- set-time");
      Ada.Text_IO.Put_Line ("controller:"  & Args.Controller.ID'Image);
      Ada.Text_IO.Put_Line ("            " & Image (R));
      Ada.Text_IO.Put_Line ("");
   end Set_Time;

end Handlers;
