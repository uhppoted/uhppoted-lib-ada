with GNAT.Command_Line;
with GNAT.Regpat;
with GNAT.Strings;

package body ArgParse is
   use GNAT.Command_Line;
   use GNAT.Regpat;
   use GNAT.Sockets;
   use GNAT.Strings;

   use Uhppoted.Lib;

   function Parse (Cmd : String) return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;

      Listener_Addr        : aliased String_Access := null;
      Listener_Interval    : aliased Integer       := 0;

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

      if Cmd = "set-listener" then
         Define_Switch (Config,
                        Section => "set-listener",
                        Output      => Listener_Addr'Access,
                        Long_Switch => "--listener:",
                        Help        => "event listener IPv4 address:port",
                        Argument    => "ADDRESS");

         Define_Switch (Config,
                        Section => "set-listener",
                        Output      => Listener_Interval'Access,
                        Long_Switch => "--interval:",
                        Help        => "auto-send interval (seconds)",
                        Argument    => "SECONDS");
      end if;

      Getopt (Config, Section => Cmd, Concatenate => True);

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

      --  return command specific args
      if Cmd = "set-listener" then
         declare
            Re      : constant Pattern_Matcher := Compile ("^([0-9.]+)(:([0-9]+))?$");
            Matches : Match_Array (0 .. 3);
            S       : String renames Listener_Addr.all;

            AddrPort  : Sock_Addr_Type := (Family => Family_Inet,
                                           Addr   => Inet_Addr ("192.168.1.100"),
                                           Port   => 60001);
         begin
            Match (Re, S, Matches);

            if Matches (1) /= No_Match and then Matches (3) /= No_Match then
               AddrPort := (Family => Family_Inet,
                            Addr   => Inet_Addr (S (Matches (1).First .. Matches (1).Last)),
                            Port   => Port_Type'Value (S (Matches (3).First .. Matches (3).Last)));
            elsif Matches (1) /= No_Match then
               AddrPort := (Family => Family_Inet,
                            Addr   => Inet_Addr (S (Matches (1).First .. Matches (1).Last)),
                            Port   => 60001);
            end if;

            return (T          => ArgParse.Set_Listener_Args,
                    Controller => (ID       => Unsigned_32 (Controller_ID),
                                   DestAddr => DestAddr,
                                   Protocol => Transport),
                    Listener   => AddrPort,
                    Interval   => Unsigned_8 (Listener_Interval));
         end;
      end if;

      return (T          => ArgParse.General_Args,
              Controller => (ID       => Unsigned_32 (Controller_ID),
                             DestAddr => DestAddr,
                             Protocol => Transport));
   end Parse;

end ArgParse;
