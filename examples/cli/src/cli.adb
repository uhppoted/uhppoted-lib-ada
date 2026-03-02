with Ada.Command_Line;
with Registry;

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
--  with GNAT.Strings;      use GNAT.Strings;

procedure CLI is
   use Ada.Command_Line;

   Commands : constant Registry.Command_Set := Registry.Initialise;

   Config  : Command_Line_Configuration;
   --  Verbose : aliased Boolean := False;
   --  Output  : aliased GNAT.Strings.String_Access;
   Controller  : aliased Integer := 0;

begin
   --  Define_Switch (Config,
   --                 Output  => Verbose'Access,
   --                 Switch  => "-v",
   --                 Long_Switch => "--verbose",
   --                 Help    => "Enable verbose output");
   --
   --  Define_Switch (Config,
   --                 Output  => Output'Access,
   --                 Switch  => "-c:",
   --                 Long_Switch => "--controller:",
   --                 Help    => "Output file name",
   --                 Argument => "FILE");

   Define_Switch (Config,
                  Output  => Controller'Access,
                  Switch  => "-c:",
                  Long_Switch => "--controller:",
                  Help    => "controller serial number",
                  Argument => "N");

   Getopt (Config);
   --  if Verbose then
   --     Put_Line ("Verbose mode ON");
   --  end if;
   --
   --  if Output /= null then
   --     Put_Line ("Output file: " & Output.all);
   --     GNAT.Strings.Free (Output);   -- free when done
   --  end if;

   Put_Line (" >>> controller: " & Controller'Image);

   if Argument_Count > 0 then
      declare
         Cmd : constant String := Argument (1);
      begin
         Commands.Execute (Cmd, "woot");
      end;
   end if;

end CLI;
