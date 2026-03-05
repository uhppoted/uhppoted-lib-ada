with Ada.Command_Line;
with Registry;
with ArgParse;

procedure CLI is
   use Ada.Command_Line;

   Commands : constant Registry.Command_Set := Registry.Initialise;

begin
   if Argument_Count > 0 then
      declare
         Cmd  : constant String        := Argument (1);
         Args : constant ArgParse.Args := ArgParse.Parse (Cmd);
      begin
         Commands.Execute (Cmd, Args);
      end;
   end if;

end CLI;
