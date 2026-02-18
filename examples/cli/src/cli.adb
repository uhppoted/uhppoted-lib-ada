with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Handlers;

procedure CLI is
   use Ada.Text_IO;
   use Ada.Command_Line;

   type Handler is access procedure;

   package Command_Map is new Ada.Containers.Indefinite_Hashed_Maps (
      Key_Type        => String,
      Element_Type    => Handler,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Commands : Command_Map.Map;

   procedure Register_Commands is
   begin
      Commands.Insert ("find-controllers", Handlers.Find_Controllers'Access);
      Commands.Insert ("get-controller",   Handlers.Get_Controller'Access);
   end Register_Commands;

begin
   Register_Commands;

   if Argument_Count > 0 then
      declare
         Cmd : constant String := Argument (1);
      begin
         if Commands.Contains (Cmd) then
            Commands.Element (Cmd).all;
         else
            Put_Line ("   *** ERROR: unknown command " & Cmd);
         end if;
      end;
   end if;

end CLI;
