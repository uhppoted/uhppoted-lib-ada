with Ada.Text_IO;
with Uhppoted.Lib;

procedure Cli is
   use Uhppoted.Lib;

   Controllers : constant Controller_List := Find_Controllers;
begin
   Ada.Text_IO.Put_Line ("--- UHPPOTE Discovery Test ---");

   if Controllers'Length = 0 then
      Ada.Text_IO.Put_Line ("No controllers found.");
   else
      for C of Controllers loop
         Ada.Text_IO.Put_Line ("Found Controller ID:" & C.ID'Image);
         Ada.Text_IO.Put_Line ("                     " & Image (C.Address));
         Ada.Text_IO.Put_Line ("                     " & Image (C.Netmask));
         Ada.Text_IO.Put_Line ("                     " & Image (C.Gateway));
         Ada.Text_IO.Put_Line ("                     " & Image (C.MAC));
         Ada.Text_IO.Put_Line ("                     " & C.Firmware);
         Ada.Text_IO.Put_Line ("                     " & Image (C.Date));
      end loop;
   end if;

   Ada.Text_IO.Put_Line ("------------------------------");
end Cli;
