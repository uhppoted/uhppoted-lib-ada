with GNAT.Command_Line;
with GNAT.Sockets;

package body ArgParse is
   use Interfaces;
   use GNAT.Command_Line;
   use GNAT.Sockets;
   use Uhppoted.Lib;

   function Parse (Cmd : String) return Args is
      Config     : Command_Line_Configuration;
      Controller : aliased Integer := 0;
   begin
      Define_Switch (Config,
                     Output  => Controller'Access,
                     Switch  => "-c:",
                     Long_Switch => "--controller:",
                     Help    => "controller serial number",
                     Argument => "N");

      Getopt (Config);

      return (T => ArgParse.Find_Controllers_Args,
              Controller => (
                 ID       => Unsigned_32 (Controller),
                 DestAddr => (Family => Family_Inet, Addr => Inet_Addr ("192.168.1.125"), Port => 60000),
                 Protocol => TCP));

   end Parse;

end ArgParse;

--   Config  : Command_Line_Configuration;
--   --  Verbose : aliased Boolean := False;
--   --  Output  : aliased GNAT.Strings.String_Access;
--   Controller  : aliased Integer := 0;
--
--
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
--
--  Define_Switch (Config,
--                 Output  => Controller'Access,
--                 Switch  => "-c:",
--                 Long_Switch => "--controller:",
--                 Help    => "controller serial number",
--                 Argument => "N");
--
--  Getopt (Config);
--  if Verbose then
--     Put_Line ("Verbose mode ON");
--  end if;
--
--  if Output /= null then
--     Put_Line ("Output file: " & Output.all);
--     GNAT.Strings.Free (Output);   -- free when done
--  end if;

