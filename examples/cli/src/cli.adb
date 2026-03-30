with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Registry;
with ArgParse;

with Uhppoted.Lib;

procedure CLI is
   use Ada.Text_IO;
   use Ada.Command_Line;
   use Ada.Exceptions;

   Commands : constant Registry.Command_Set := Registry.Initialise;

begin
   if Argument_Count > 0 then
      declare
         Cmd  : constant String        := Argument (1);
         Args : constant ArgParse.Args := ArgParse.Parse (Cmd);
      begin
         Commands.Execute (Cmd, Args);
      exception
         when Uhppoted.Lib.Timeout_Error =>
            Put_Line("    *** ERROR timeout waiting for response");

         when Uhppoted.Lib.Invalid_Response_Error =>
            Put_Line("    *** ERROR invalid response from controller");

         when E : others =>
            Put_Line("    *** ERROR " & Exception_Name(E) & "  " & Exception_Message(E));
      end;
   end if;

end CLI;
