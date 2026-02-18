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

   C : constant Controller := (
      Controller => 405419896,
      Address    => ( Family => GNAT.Sockets.Family_Inet, Addr => Inet_Addr ("192.168.1.125"), Port => 60000),
      Protocol   => Default);

   Controllers : constant Controller_Record_List := Find_Controllers (U);
   C1 : constant Controller_Record := Get_Controller (U, 405419896);
   C2 : constant Controller_Record := Get_Controller (U, C);

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
   Ada.Text_IO.Put_Line ("controller:" & C1.ID'Image);
   Ada.Text_IO.Put_Line ("            " & Image (C1.Address));
   Ada.Text_IO.Put_Line ("            " & Image (C1.Netmask));
   Ada.Text_IO.Put_Line ("            " & Image (C1.Gateway));
   Ada.Text_IO.Put_Line ("            " & Image (C1.MAC));
   Ada.Text_IO.Put_Line ("            " & C1.Firmware);
   Ada.Text_IO.Put_Line ("            " & Image (C1.Date));

   Ada.Text_IO.Put_Line ("");

   Ada.Text_IO.Put_Line ("--- get-controller");
   Ada.Text_IO.Put_Line ("controller:" & C2.ID'Image);
   Ada.Text_IO.Put_Line ("            " & Image (C2.Address));
   Ada.Text_IO.Put_Line ("            " & Image (C2.Netmask));
   Ada.Text_IO.Put_Line ("            " & Image (C2.Gateway));
   Ada.Text_IO.Put_Line ("            " & Image (C2.MAC));
   Ada.Text_IO.Put_Line ("            " & C2.Firmware);
   Ada.Text_IO.Put_Line ("            " & Image (C2.Date));

   Ada.Text_IO.Put_Line ("");

end Cli;
