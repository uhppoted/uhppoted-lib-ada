with Ada.Text_IO;
with Handlers;

package body Registry is
   use Ada.Text_IO;
   use Handlers;

   function Initialise return Command_Set is
      CS : Command_Set;
   begin
      CS.Internal_Map.Insert ("find-controllers", Find_Controllers'Access);
      CS.Internal_Map.Insert ("get-controller",   Get_Controller'Access);
      CS.Internal_Map.Insert ("set-IPv4",         Set_IPv4'Access);

      return CS;
   end Initialise;

   procedure Execute (Self : Command_Set; Cmd : String) is
   begin
         if Self.Internal_Map.Contains (Cmd) then
            Self.Internal_Map.Element (Cmd).all;
         else
            Put_Line ("   *** ERROR: unknown command " & Cmd);
         end if;
   end Execute;

end Registry;
