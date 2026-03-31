with Ada.Text_IO;
with Handlers;

package body Registry is
   use Ada.Text_IO;
   use Handlers;

   function Initialise return Command_Set is
      CS : Command_Set;
   begin
      CS.Internal_Map.Insert ("find-controllers",   Find_Controllers'Access);
      CS.Internal_Map.Insert ("get-controller",     Get_Controller'Access);
      CS.Internal_Map.Insert ("set-IPv4",           Set_IPv4'Access);
      CS.Internal_Map.Insert ("get-time",           Get_Time'Access);
      CS.Internal_Map.Insert ("set-time",           Set_Time'Access);
      CS.Internal_Map.Insert ("get-listener",       Get_Listener'Access);
      CS.Internal_Map.Insert ("set-listener",       Set_Listener'Access);
      CS.Internal_Map.Insert ("get-status",         Get_Status'Access);
      CS.Internal_Map.Insert ("get-door",           Get_Door'Access);
      CS.Internal_Map.Insert ("set-door",           Set_Door'Access);
      CS.Internal_Map.Insert ("set-door-passcodes", Set_Door_Passcodes'Access);
      CS.Internal_Map.Insert ("open-door",          Open_Door'Access);

      return CS;
   end Initialise;

   procedure Execute (Self : Command_Set;
                      Cmd  : String;
                      Args : ArgParse.Args) is
   begin
      if Self.Internal_Map.Contains (Cmd) then
         Self.Internal_Map.Element (Cmd).all (Args);
      else
         Put_Line ("   *** ERROR: unknown command " & Cmd);
      end if;
   end Execute;

end Registry;
