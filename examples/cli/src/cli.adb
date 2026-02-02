with Ada.Text_IO;
with Uhppoted.Lib;

procedure Cli is
   use Uhppoted.Lib;
   
   Controllers : constant Controller_Array := Get_Controllers;
begin
   Ada.Text_IO.Put_Line ("--- UHPPOTE Discovery Test ---");
   
   if Controllers'Length = 0 then
      Ada.Text_IO.Put_Line ("No controllers found.");
   else
      for ID of Controllers loop
         Ada.Text_IO.Put_Line ("Found Controller ID:" & ID'Image);
      end loop;
   end if;

   Ada.Text_IO.Put_Line ("------------------------------");
end Cli;
