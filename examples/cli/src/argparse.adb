with Ada.Strings.Fixed;
with GNAT.Regpat;

package body ArgParse is
   use GNAT.Regpat;

   use Uhppoted.Lib;

   function Parse (Cmd : String) return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;

      C : Controller;
   begin
      --  return command specific args
      if Cmd = "set-IPv4" then
         return Parse_Set_IPv4;
      end if;

      if Cmd = "set-listener" then
         return Parse_Set_Listener;
      end if;

      if Cmd = "get-door" then
         return Parse_Get_Door;
      end if;

      if Cmd = "set-door" then
         return Parse_Set_Door;
      end if;

      if Cmd = "set-door-passcodes" then
         return Parse_Set_Door_Passcodes;
      end if;

      if Cmd = "open-door" then
         return Parse_Open_Door;
      end if;

      if Cmd = "get-card" then
         return Parse_Get_Card;
      end if;

      if Cmd = "get-card-at-index" then
         return Parse_Get_Card_At_Index;
      end if;

      --  default to general command
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);
      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      return (T          => ArgParse.General_Args,
              Controller => C,
              Door       => 0,
              Card       => 0);
   end Parse;

   procedure Add_Controller_Switches (Config               : in out Command_Line_Configuration;
                                      Controller_ID        : access Integer;
                                      Controller_Addr      : access String_Access;
                                      Controller_Transport : access String_Access) is
   begin
      Define_Switch (Config,
                     Output      => Controller_ID,
                     Long_Switch => "--controller:",
                     Help        => "controller serial number",
                     Argument    => "N");
      Define_Switch (Config,
                     Output      => Controller_Addr,
                     Long_Switch => "--dest:",
                     Help        => "controller IPv4 address",
                     Argument    => "ADDRESS");
      Define_Switch (Config,
                     Output      => Controller_Transport,
                     Long_Switch => "--protocol:",
                     Help        => "controller protocol ('udp' or 'tcp')",
                     Argument    => "[UDP | TCP]");
   end Add_Controller_Switches;

   procedure Extract_Controller_Args (Controller_ID        : Integer;
                                      Controller_Addr      : String_Access;
                                      Controller_Transport : String_Access;
                                      C                    : out Controller) is
      Re      : constant Pattern_Matcher := Compile ("^([0-9.]+)(:([0-9]+))?$");
      Matches : Match_Array (0 .. 3);
   begin
      C := (ID       => Unsigned_32 (Controller_ID),
            DestAddr => No_Sock_Addr,
            Protocol => Default);

      if Controller_Addr /= null and then Controller_Addr.all /= "" then
         declare
            S : String renames Controller_Addr.all;
         begin
            Match (Re, S, Matches);
            if Matches (1) /= No_Match and then Matches (3) /= No_Match then
               C.DestAddr := (Family => Family_Inet,
                              Addr   => Inet_Addr (S (Matches (1).First .. Matches (1).Last)),
                              Port   => Port_Type'Value (S (Matches (3).First .. Matches (3).Last)));
            elsif Matches (1) /= No_Match then
               C.DestAddr := (Family => Family_Inet,
                              Addr   => Inet_Addr (S (Matches (1).First .. Matches (1).Last)),
                              Port   => 60000);
            end if;
         end;
      end if;

      if Controller_Transport /= null then
         if Controller_Transport.all = "udp" then
            C.Protocol := UDP;
         elsif Controller_Transport.all = "tcp" then
            C.Protocol := TCP;
         end if;
      end if;

   end Extract_Controller_Args;

   --  set-IPv4 specific args
   function Parse_Set_IPv4 return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;

      Address : aliased String_Access := null;
      Netmask : aliased String_Access := null;
      Gateway : aliased String_Access := null;

      C : Controller;

   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Address'Access,
                     Long_Switch => "--address:",
                     Help        => "IPv4 address",
                     Argument    => "ADDRESS");

      Define_Switch (Config,
                     Output      => Netmask'Access,
                     Long_Switch => "--netmask:",
                     Help        => "IPv4 subnet mask",
                     Argument    => "NETMASK");

      Define_Switch (Config,
                     Output      => Gateway'Access,
                     Long_Switch => "--gateway:",
                     Help        => "gateway IPv4address",
                     Argument    => "GATEWAY");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      declare
         A : String renames Address.all;
         M : String renames Netmask.all;
         G : String renames Gateway.all;
      begin
         return (T          => ArgParse.Set_IPv4_Args,
                 Controller => C,
                 Door       => 0,
                 Card       => 0,
                 Address    => Inet_Addr (A),
                 Netmask    => Inet_Addr (M),
                 Gateway    => Inet_Addr (G));
      end;

   end Parse_Set_IPv4;

   --  set-listener specific args
   function Parse_Set_Listener return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;

      Listener_Addr     : aliased String_Access := null;
      Listener_Interval : aliased Integer       := 0;

      C        : Controller;
      AddrPort : Sock_Addr_Type := (Family => Family_Inet,
                                     Addr   => Inet_Addr ("192.168.1.100"),
                                     Port   => 60001);

   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Listener_Addr'Access,
                     Long_Switch => "--listener:",
                     Help        => "event listener IPv4 address:port",
                     Argument    => "ADDRESS");

      Define_Switch (Config,
                     Output      => Listener_Interval'Access,
                     Long_Switch => "--interval:",
                     Help        => "auto-send interval (seconds)",
                     Argument    => "SECONDS");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  set-listener specific args
      declare
         Re      : constant Pattern_Matcher := Compile ("^([0-9.]+)(:([0-9]+))?$");
         Matches : Match_Array (0 .. 3);
         S       : String renames Listener_Addr.all;
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
      end;

      return (T          => ArgParse.Set_Listener_Args,
              Controller => C,
              Door       => 0,
              Card       => 0,
              Listener   => AddrPort,
              Interval   => Unsigned_8 (Listener_Interval));

   end Parse_Set_Listener;

   function Parse_Get_Door return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Door                 : aliased Integer       := 0;

      C : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Door'Access,
                     Long_Switch => "--door:",
                     Help        => "door ID [1..4]",
                     Argument    => "[1..4]");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return get-door command specific args
      return (T          => ArgParse.Get_Door_Args,
              Controller => C,
              Door       => Unsigned_8 (Door),
              Card       => 0);

   end Parse_Get_Door;

   function Parse_Set_Door return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Door                 : aliased Integer       := 0;
      Mode                 : aliased String_Access := null;
      OpenDelay            : aliased Integer       := 0;

      C : Controller;
      M : Control_Mode := Controlled;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Door'Access,
                     Long_Switch => "--door:",
                     Help        => "door ID [1..4]",
                     Argument    => "[1..4]");

      Define_Switch (Config,
                     Output      => Mode'Access,
                     Long_Switch => "--mode:",
                     Help        => "door control mode",
                     Argument    => "[controlled | normally-open | normally-closed]");

      Define_Switch (Config,
                     Output      => OpenDelay'Access,
                     Long_Switch => "--delay:",
                     Help        => "door open delay",
                     Argument    => "SECONDS");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return set-door command specific args
      declare
         S : String renames Mode.all;
      begin
         if S = "normally-open" then
            M := Normally_Open;
         elsif S = "normally-closed" then
            M := Normally_Closed;
         elsif S = "controlled" then
            M := Controlled;
         end if;
      end;

      return (T          => ArgParse.Set_Door_Args,
              Controller => C,
              Door       => Unsigned_8 (Door),
              Mode       => M,
              OpenDelay  => Unsigned_8 (OpenDelay),
              Card       => 0);

   end Parse_Set_Door;

   function Parse_Set_Door_Passcodes return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Door                 : aliased Integer       := 0;
      Passcodes            : aliased String_Access := null;

      C     : Controller;
      Codes : Passcodes_List (1 .. 4) := [0, 0, 0, 0];
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Door'Access,
                     Long_Switch => "--door:",
                     Help        => "door ID [1..4]",
                     Argument    => "[1..4]");

      Define_Switch (Config,
                     Output      => Passcodes'Access,
                     Long_Switch => "--passcodes:",
                     Help        => "supervisor override passcodes",
                     Argument    => "<passcode>,...");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return set-door-passcodes command specific args
      declare
         S     : String renames Passcodes.all;
         First : Natural  := S'First;
         Last  : Natural  := 0;
      begin
         for I in 1 .. 4 loop
            exit when First > S'Last;

            Last := Ada.Strings.Fixed.Index (S (First .. S'Last), ",");
            if Last = 0 then
               Codes (I) := Unsigned_32'Value (S (First .. S'Last));
               exit;
            else
               Codes (I) := Unsigned_32'Value (S (First .. Last - 1));
               First := Last + 1;
            end if;
         end loop;
      end;

      return (T          => ArgParse.Set_Door_Passcodes_Args,
              Controller => C,
              Door       => Unsigned_8 (Door),
              Passcodes  => Codes,
              Card       => 0);

   end Parse_Set_Door_Passcodes;

   function Parse_Open_Door return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Door                 : aliased Integer       := 0;

      C : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Door'Access,
                     Long_Switch => "--door:",
                     Help        => "door ID [1..4]",
                     Argument    => "[1..4]");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return open-door command specific args
      return (T          => ArgParse.Open_Door_Args,
              Controller => C,
              Door       => Unsigned_8 (Door),
              Card       => 0);

   end Parse_Open_Door;

   function Parse_Get_Card return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Card                 : aliased Integer       := 0;

      C : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Card'Access,
                     Long_Switch => "--card:",
                     Help        => "card number",
                     Argument    => "CARD");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return get-card command specific args
      return (T          => ArgParse.Get_Card_Args,
              Controller => C,
              Door       => 0,
              Card       => Unsigned_32 (Card));

   end Parse_Get_Card;

   function Parse_Get_Card_At_Index return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Index                : aliased Integer       := 0;

      C : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Index'Access,
                     Long_Switch => "--index:",
                     Help        => "card index",
                     Argument    => "INDEX");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return get-card command specific args
      return (T          => ArgParse.Get_Card_At_Index_Args,
              Controller => C,
              Door       => 0,
              Card       => 0,
              Card_Index => Unsigned_32 (Index));

   end Parse_Get_Card_At_Index;

end ArgParse;
