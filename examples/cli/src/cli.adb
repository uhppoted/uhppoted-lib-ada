with GNAT.Sockets;
with Ada.Text_IO;
with Uhppoted.Lib;

procedure Cli is
   use Uhppoted.Lib;
   use GNAT.Sockets;

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

   Controllers : constant Controller_List := Find_Controllers (U);
   C : constant Controller := Get_Controller (U, 405419896);
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
         Ada.Text_IO.Put_Line ("            " & C.Firmware);
         Ada.Text_IO.Put_Line ("            " & Image (C.Date));
         Ada.Text_IO.Put_Line ("");
      end loop;
   end if;

   Ada.Text_IO.Put_Line ("");

   Ada.Text_IO.Put_Line ("--- get-controller");
   Ada.Text_IO.Put_Line ("controller:" & C.ID'Image);
   Ada.Text_IO.Put_Line ("            " & Image (C.Address));
   Ada.Text_IO.Put_Line ("            " & Image (C.Netmask));
   Ada.Text_IO.Put_Line ("            " & Image (C.Gateway));
   Ada.Text_IO.Put_Line ("            " & Image (C.MAC));
   Ada.Text_IO.Put_Line ("            " & C.Firmware);
   Ada.Text_IO.Put_Line ("            " & Image (C.Date));

   Ada.Text_IO.Put_Line ("");

end Cli;
