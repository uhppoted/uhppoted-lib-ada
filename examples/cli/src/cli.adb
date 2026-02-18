with Ada.Command_Line;

with Registry;

procedure CLI is
   use Ada.Command_Line;

   Commands : constant Registry.Command_Set := Registry.Initialise;
begin
   if Argument_Count > 0 then
      declare
         Cmd : constant String := Argument (1);
      begin
         Commands.Execute (Cmd);
      end;
   end if;

end CLI;
