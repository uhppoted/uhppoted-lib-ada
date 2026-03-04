with Interfaces;
with GNAT.Sockets;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Uhppoted.Lib;

package body Handlers is
   use Interfaces;
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

   C : constant Controller := (
      ID       => 405419896,
      DestAddr => (Family => GNAT.Sockets.Family_Inet, Addr => Inet_Addr ("192.168.1.125"), Port => 60000),
      Protocol => TCP);

   Timeout : constant Duration := 2.5;

   procedure Find_Controllers (Args : String) is
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

   procedure Get_Controller (Args : String) is
      pragma Unreferenced (Args);

      C1 : constant Controller_Record := Get_Controller (U, 405419896, Timeout);
      C2 : constant Controller_Record := Get_Controller (U, C, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- get-controller");
      Ada.Text_IO.Put_Line ("controller:" & C1.ID'Image);
      Ada.Text_IO.Put_Line ("            " & Image (C1.Address));
      Ada.Text_IO.Put_Line ("            " & Image (C1.Netmask));
      Ada.Text_IO.Put_Line ("            " & Image (C1.Gateway));
      Ada.Text_IO.Put_Line ("            " & Image (C1.MAC));
      Ada.Text_IO.Put_Line ("            " & To_String (C1.Firmware));
      Ada.Text_IO.Put_Line ("            " & Image (C1.Date));

      Ada.Text_IO.Put_Line ("");

      Ada.Text_IO.Put_Line ("--- get-controller");
      Ada.Text_IO.Put_Line ("controller:" & C2.ID'Image);
      Ada.Text_IO.Put_Line ("            " & Image (C2.Address));
      Ada.Text_IO.Put_Line ("            " & Image (C2.Netmask));
      Ada.Text_IO.Put_Line ("            " & Image (C2.Gateway));
      Ada.Text_IO.Put_Line ("            " & Image (C2.MAC));
      Ada.Text_IO.Put_Line ("            " & To_String (C2.Firmware));
      Ada.Text_IO.Put_Line ("            " & Image (C2.Date));

      Ada.Text_IO.Put_Line ("");
   end Get_Controller;

   procedure Set_IPv4 (Args : String) is
      pragma Unreferenced (Args);

      C       : constant Unsigned_32 := 405419896;
      Addr    : constant Inet_Addr_Type := Inet_Addr ("192.168.1.125");
      Netmask : constant Inet_Addr_Type := Inet_Addr ("255.255.255.0");
      Gateway : constant Inet_Addr_Type := Inet_Addr ("192.168.1.1");
      R       : constant Boolean := Set_IPv4 (U, C, Addr, Netmask, Gateway, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- set-IPv4");
      Ada.Text_IO.Put_Line ("controller:" & C'Image);
      Ada.Text_IO.Put_Line ("            " & R'Image);

      Ada.Text_IO.Put_Line ("");
   end Set_IPv4;

   procedure Get_Time (Args : String) is
      pragma Unreferenced (Args);

      R : constant DateTime := Get_Time (U, 405419896, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- get-time");
      Ada.Text_IO.Put_Line ("controller: " & "405419896");
      Ada.Text_IO.Put_Line ("            " & Image (R));

      Ada.Text_IO.Put_Line ("");
   end Get_Time;

   procedure Set_Time (Args : String) is
      pragma Unreferenced (Args);

      DT : constant DateTime := (2026, 3, 4, 12, 5, 9);
      R  : constant DateTime := Uhppoted.Lib.Set_Time (U, C, DT, Timeout);
   begin
      Ada.Text_IO.Put_Line ("--- set-time");
      Ada.Text_IO.Put_Line ("controller:" & C.ID'Image);
      Ada.Text_IO.Put_Line ("            " & Image (R));

      Ada.Text_IO.Put_Line ("");
   end Set_Time;

end Handlers;
