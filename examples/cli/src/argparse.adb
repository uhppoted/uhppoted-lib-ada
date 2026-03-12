with Interfaces;
with GNAT.Command_Line;
with GNAT.Regpat;
with GNAT.Sockets;
with GNAT.Strings;

package body ArgParse is
   use Interfaces;
   use GNAT.Command_Line;
   use GNAT.Regpat;
   use GNAT.Sockets;
   use GNAT.Strings;

   use Uhppoted.Lib;

   function Parse (Cmd : String) return Args is
      pragma Unreferenced (Cmd);

      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer                    := 0;
      Controller_Addr      : aliased GNAT.Strings.String_Access := null;
      Controller_Transport : aliased GNAT.Strings.String_Access := null;

      DestAddr  : Sock_Addr_Type := No_Sock_Addr;
      Transport : Protocol_Type  := Default;

   begin
      Define_Switch (Config,
                     Output      => Controller_ID'Access,
                     Long_Switch => "--controller:",
                     Help        => "controller serial number",
                     Argument    => "N");

      Define_Switch (Config,
                     Output      => Controller_Addr'Access,
                     Long_Switch => "--dest:",
                     Help        => "controller IPv4 address",
                     Argument    => "ADDRESS");

      Define_Switch (Config,
                     Output      => Controller_Transport'Access,
                     Long_Switch => "--protocol:",
                     Help        => "controller protocol ('udp' or 'tcp')",
                     Argument    => "[UDP | TCP]");

      Getopt (Config);

      --  get controller address
      if Controller_Addr /= null and then Controller_Addr.all /= "" then
         declare
            Re      : constant Pattern_Matcher := Compile ("^([0-9.]+)(:([0-9]+))?$");
            Matches : Match_Array (0 .. 3);
            S       : String renames Controller_Addr.all;
         begin
            Match (Re, S, Matches);

            if Matches (1) /= No_Match and then Matches (3) /= No_Match then
               DestAddr := (Family => Family_Inet,
                            Addr   => Inet_Addr (S (Matches (1).First .. Matches (1).Last)),
                            Port   => Port_Type'Value (S (Matches (3).First .. Matches (3).Last)));
            elsif Matches (1) /= No_Match then
               DestAddr := (Family => Family_Inet,
                            Addr   => Inet_Addr (S (Matches (1).First .. Matches (1).Last)),
                            Port   => 60000);
            end if;
         end;
      end if;

      --  get controller transport
      if Controller_Transport /= null and then Controller_Transport.all = "udp" then
         Transport := UDP;
      elsif Controller_Transport /= null and then Controller_Transport.all = "tcp" then
         Transport := TCP;
      end if;

      return (T          => ArgParse.Find_Controllers_Args,
              Controller => (ID       => Unsigned_32 (Controller_ID),
                             DestAddr => DestAddr,
                             Protocol => Transport));
   end Parse;

end ArgParse;
