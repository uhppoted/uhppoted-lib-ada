with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with GNAT.Regpat;

package body ArgParse is
   use GNAT.Regpat;
   use Ada.Characters.Handling;

   use Uhppoted.Lib;

   Invalid_Argument : exception;

   function To_HHmm (S : String) return Uhppoted.Lib.HHmm is
   begin
      return (Hour   => Unsigned_8'Value (S (S'First .. S'First + 1)),
              Minute => Unsigned_8'Value (S (S'First + 3 .. S'First + 4)));
   end To_HHmm;

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
      elsif Cmd = "set-listener" then
         return Parse_Set_Listener;
      elsif Cmd = "get-door" then
         return Parse_Get_Door;
      elsif Cmd = "set-door" then
         return Parse_Set_Door;
      elsif Cmd = "set-door-passcodes" then
         return Parse_Set_Door_Passcodes;
      elsif Cmd = "open-door" then
         return Parse_Open_Door;
      elsif Cmd = "get-card" then
         return Parse_Get_Card;
      elsif Cmd = "get-card-at-index" then
         return Parse_Get_Card_At_Index;
      elsif Cmd = "put-card" then
         return Parse_Put_Card;
      elsif Cmd = "delete-card" then
         return Parse_Delete_Card;
      elsif Cmd = "get-event" then
         return Parse_Get_Event;
      elsif Cmd = "set-event-index" then
         return Parse_Set_Event_Index;
      elsif Cmd = "record-special-events" then
         return Parse_Record_Special_Events;
      elsif Cmd = "get-time-profile" then
         return Parse_Get_Time_Profile;
      elsif Cmd = "set-time-profile" then
         return Parse_Set_Time_Profile;
      elsif Cmd = "add-task" then
         return Parse_Add_Task;
      end if;

      --  default to general command
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);
      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      return (T           => ArgParse.General_Args,
              Controller  => C,
              Door        => 0,
              Card        => (others => <>),
              Event_Index => 0,
              Profile_ID  => 0);
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
         return (T           => ArgParse.Set_IPv4_Args,
                 Controller  => C,
                 Door        => 0,
                 Card        => (others => <>),
                 Event_Index => 0,
                 Profile_ID  => 0,
                 Address     => Inet_Addr (A),
                 Netmask     => Inet_Addr (M),
                 Gateway     => Inet_Addr (G));
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

      return (T           => ArgParse.Set_Listener_Args,
              Controller  => C,
              Door        => 0,
              Card        => (others => <>),
              Event_Index => 0,
              Profile_ID  => 0,
              Listener    => AddrPort,
              Interval    => Unsigned_8 (Listener_Interval));

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
      return (T           => ArgParse.Get_Door_Args,
              Controller  => C,
              Door        => Unsigned_8 (Door),
              Card        => (others => <>),
              Event_Index => 0,
              Profile_ID  => 0);

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

      return (T           => ArgParse.Set_Door_Args,
              Controller  => C,
              Door        => Unsigned_8 (Door),
              Mode        => M,
              OpenDelay   => Unsigned_8 (OpenDelay),
              Card        => (others => <>),
              Event_Index => 0,
              Profile_ID  => 0);

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

      return (T           => ArgParse.Set_Door_Passcodes_Args,
              Controller  => C,
              Door        => Unsigned_8 (Door),
              Passcodes   => Codes,
              Card        => (others => <>),
              Event_Index => 0,
              Profile_ID  => 0);

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
      return (T           => ArgParse.Open_Door_Args,
              Controller  => C,
              Door        => Unsigned_8 (Door),
              Card        => (others => <>),
              Event_Index => 0,
              Profile_ID  => 0);

   end Parse_Open_Door;

   function Parse_Get_Card return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Card                 : aliased Integer       := 0;

      C  : Controller;
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
      return (T           => ArgParse.Get_Card_Args,
              Controller  => C,
              Door        => 0,
              Card        => (Card => Unsigned_32 (Card), others => <>),
              Event_Index => 0,
              Profile_ID  => 0);

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
      return (T           => ArgParse.Get_Card_At_Index_Args,
              Controller  => C,
              Door        => 0,
              Card        => (others => <>),
              Card_Index  => Unsigned_32 (Index),
              Event_Index => 0,
              Profile_ID  => 0);

   end Parse_Get_Card_At_Index;

   function Parse_Put_Card return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;

      Card       : aliased Integer       := 0;
      Start_Date : aliased String_Access := null;
      End_Date   : aliased String_Access := null;
      Doors      : aliased String_Access := null;
      PIN        : aliased Integer       := 0;

      C : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Card'Access,
                     Long_Switch => "--card:",
                     Help        => "card number",
                     Argument    => "CARD");

      Define_Switch (Config,
                     Output      => Start_Date'Access,
                     Long_Switch => "--start:",
                     Help        => "start date",
                     Argument    => "YYYY-MM-DD");

      Define_Switch (Config,
                     Output      => End_Date'Access,
                     Long_Switch => "--end:",
                     Help        => "end date",
                     Argument    => "YYYY-MM-DD");

      Define_Switch (Config,
                     Output      => Doors'Access,
                     Long_Switch => "--doors:",
                     Help        => "e.g. 1,3,4:19",
                     Argument    => "DOORS");

      Define_Switch (Config,
                     Output      => PIN'Access,
                     Long_Switch => "--PIN:",
                     Help        => "(optional) PIN",
                     Argument    => "PIN [0..999999]");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return put-card command specific args
      declare
         S : String renames Start_Date.all;
         E : String renames End_Date.all;
         D : String renames Doors.all;

         I : Positive := D'First;

         Door_1 : Unsigned_8 := 0;
         Door_2 : Unsigned_8 := 0;
         Door_3 : Unsigned_8 := 0;
         Door_4 : Unsigned_8 := 0;
      begin
         --  parse 'doors' arg
         while I <= D'Last loop
            declare
               J     : constant Natural := Ada.Strings.Fixed.Index (D, ",", I);
               Token : constant String  := (if J = 0 then D (I .. D'Last) else D (I .. J - 1));
               V     : constant Integer := Integer'Value (Token);
            begin
               I := (if J = 0 then D'Last + 1 else J + 1);

               case V is
                  when 1 => Door_1 := 1;
                  when 2 => Door_2 := 1;
                  when 3 => Door_3 := 1;
                  when 4 => Door_4 := 1;
                  when others => null;
               end case;
            end;
         end loop;

         return (T           => ArgParse.Put_Card_Args,
                 Controller  => C,
                 Door        => 0,
                 Card        => (Card       => Unsigned_32 (Card),
                                 Start_Date => (Year  => Unsigned_16'Value (S (1 .. 4)),
                                                Month => Unsigned_8'Value  (S (6 .. 7)),
                                                Day   => Unsigned_8'Value  (S (9 .. 10))),
                                 End_Date   => (Year  => Unsigned_16'Value (E (1 .. 4)),
                                                Month => Unsigned_8'Value  (E (6 .. 7)),
                                                Day   => Unsigned_8'Value  (E (9 .. 10))),
                                 Door_1     => Door_1,
                                 Door_2     => Door_2,
                                 Door_3     => Door_3,
                                 Door_4     => Door_4,
                                 PIN        => Unsigned_24 (PIN)),
                 Event_Index => 0,
                 Profile_ID  => 0);
      end;

   end Parse_Put_Card;

   function Parse_Delete_Card return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Card                 : aliased Integer       := 0;

      C  : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Card'Access,
                     Long_Switch => "--card:",
                     Help        => "card number",
                     Argument    => "CARD");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return delete-card command specific args
      return (T           => ArgParse.Delete_Card_Args,
              Controller  => C,
              Door        => 0,
              Card        => (Card => Unsigned_32 (Card), others => <>),
              Event_Index => 0,
              Profile_ID  => 0);

   end Parse_Delete_Card;

   function Parse_Get_Event return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Index                : aliased Integer       := 0;

      C  : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Index'Access,
                     Long_Switch => "--index:",
                     Help        => "index",
                     Argument    => "EVENT INDEX");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return set-event-index command specific args
      return (T           => ArgParse.Set_Event_Index_Args,
              Controller  => C,
              Door        => 0,
              Card        => (others => <>),
              Event_Index => Unsigned_32 (Index),
              Profile_ID  => 0);

   end Parse_Get_Event;

   function Parse_Set_Event_Index return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Index                : aliased Integer       := 0;

      C  : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Index'Access,
                     Long_Switch => "--index:",
                     Help        => "index",
                     Argument    => "EVENT INDEX");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return set-event-index command specific args
      return (T           => ArgParse.Set_Event_Index_Args,
              Controller  => C,
              Door        => 0,
              Card        => (others => <>),
              Event_Index => Unsigned_32 (Index),
              Profile_ID  => 0);

   end Parse_Set_Event_Index;

   function Parse_Record_Special_Events return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Enabled              : aliased String_Access := null;

      C  : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Enabled'Access,
                     Long_Switch => "--enabled:",
                     Help        => "enables/disables events for e.g. door unlock",
                     Argument    => "YES/NO");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return record-special-events command specific args
      declare
         S : String renames Enabled.all;
         E : constant Boolean := S = "yes" or else S = "YES";
      begin
         return (T           => ArgParse.Record_Special_Events_Args,
                 Controller  => C,
                 Door        => 0,
                 Card        => (others => <>),
                 Event_Index => 0,
                 Profile_ID  => 0,
                 Enabled     => E);
      end;

   end Parse_Record_Special_Events;

   function Parse_Get_Time_Profile return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Profile_ID           : aliased Integer       := 0;

      C  : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Profile_ID'Access,
                     Long_Switch => "--profile-id:",
                     Help        => "time profile ID [2..254]",
                     Argument    => "[2..254]");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return get-time-profile command specific args
      return (T           => ArgParse.Get_Time_Profile_Args,
              Controller  => C,
              Door        => 0,
              Card        => (others => <>),
              Event_Index => 0,
              Profile_ID  => Unsigned_8 (Profile_ID));

   end Parse_Get_Time_Profile;

   function Parse_Set_Time_Profile return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      Profile_ID           : aliased Integer       := 0;
      Profile              : aliased String_Access := null;

      C  : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => Profile_ID'Access,
                     Long_Switch => "--profile-id:",
                     Help        => "time profile ID [2..254]",
                     Argument    => "[2..254]");

      Define_Switch (Config,
                     Output      => Profile'Access,
                     Long_Switch => "--profile:",
                     Help        => "time profile",
                     Argument    => "<start>,<end>,[<weekdays>],[<segments>],<linked>");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return set-time-profile command specific args
      declare
         S : String renames Profile.all;
         P : Time_Profile;

         I : constant Natural := Ada.Strings.Fixed.Index (S, ",", 1);
         J : constant Natural := Ada.Strings.Fixed.Index (S, ",", I + 1);
         K : constant Natural := Ada.Strings.Fixed.Index (S, "[", J + 1);
         L : constant Natural := Ada.Strings.Fixed.Index (S, "]", K + 1);
         M : constant Natural := Ada.Strings.Fixed.Index (S, "[", L + 1);
         N : constant Natural := Ada.Strings.Fixed.Index (S, "]", M + 1);
         O : constant Natural := Ada.Strings.Fixed.Index (S, ",", N + 1);

         Start_Date : constant String (1 .. I - 1)     := S (1 .. I - 1);
         End_Date   : constant String (1 .. J - I - 1) := S (I + 1 .. J - 1);
         Weekdays   : constant String (1 .. L - K - 1) := S (K + 1 .. L - 1);
         Segments   : constant String (1 .. N - M - 1) := S (M + 1 .. N - 1);
         Linked     : constant String                  := S (O + 1 .. S'Last);

         Segment_1 : Time_Segment := (Start_Time => (0, 0), End_Time => (0, 0));
         Segment_2 : Time_Segment := (Start_Time => (0, 0), End_Time => (0, 0));
         Segment_3 : Time_Segment := (Start_Time => (0, 0), End_Time => (0, 0));
      begin
         declare
            IX    : constant Natural := Ada.Strings.Fixed.Index (Segments, ",");
            Token : constant String  := (if IX = 0 then Segments else Segments (Segments'First .. IX - 1));
         begin
            if Token'Length = 11 then
               Segment_1 := (Start_Time => To_HHmm (Token (Token'First .. Token'First + Token'First + 4)),
                             End_Time   => To_HHmm (Token (Token'First + 6 .. Token'First + 10)));
            end if;

            if IX /= 0 then
               declare
                  JX    : constant Natural := Ada.Strings.Fixed.Index (Segments (IX + 1 .. Segments'Last), ",");
                  Token : constant String := (if JX = 0 then Segments (IX + 1 .. Segments'Last)
                                                        else Segments (IX + 1 .. JX - 1));
               begin
                  if Token'Length = 11 then
                     Segment_2 := (Start_Time => To_HHmm (Token (Token'First .. Token'First + 4)),
                                   End_Time   => To_HHmm (Token (Token'First + 6 .. Token'First + 10)));
                  end if;

                  if JX /= 0 then
                     declare
                        KX    : constant Natural := Ada.Strings.Fixed.Index (Segments (JX + 1 .. Segments'Last), ",");
                        Token : constant String := (if KX = 0 then Segments (JX + 1 .. Segments'Last)
                                                              else Segments (JX + 1 .. KX - 1));
                     begin
                        if Token'Length = 11 then
                           Segment_3 := (Start_Time => To_HHmm (Token (Token'First .. Token'First + 4)),
                                         End_Time   => To_HHmm (Token (Token'First + 6 .. Token'First + 10)));
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;

         P := (Start_Date => (Year  => Unsigned_16'Value (Start_Date (1 .. 4)),
                              Month => Unsigned_8'Value  (Start_Date (6 .. 7)),
                              Day   => Unsigned_8'Value  (Start_Date (9 .. 10))),
               End_Date   => (Year  => Unsigned_16'Value (End_Date (1 .. 4)),
                              Month => Unsigned_8'Value  (End_Date (6 .. 7)),
                              Day   => Unsigned_8'Value  (End_Date (9 .. 10))),
               Weekdays   => (Monday    => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "MON") > 0,
                              Tuesday   => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "TUE") > 0,
                              Wednesday => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "WED") > 0,
                              Thursday  => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "THU") > 0,
                              Friday    => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "FRI") > 0,
                              Saturday  => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "SAT") > 0,
                              Sunday    => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "SUN") > 0),
               Segments   => [1 => Segment_1,
                              2 => Segment_2,
                              3 => Segment_3],
               Linked_Profile => Unsigned_8'Value (Linked));

         return (T           => ArgParse.Set_Time_Profile_Args,
                 Controller  => C,
                 Door        => 0,
                 Card        => (others => <>),
                 Event_Index => 0,
                 Profile_ID  => Unsigned_8 (Profile_ID),
                 Profile     => P);
      end;

   end Parse_Set_Time_Profile;

   function Parse_Add_Task return Args is
      Config               : Command_Line_Configuration;
      Controller_ID        : aliased Integer       := 0;
      Controller_Addr      : aliased String_Access := null;
      Controller_Transport : aliased String_Access := null;
      TaskT                : aliased String_Access := null;

      C  : Controller;
   begin
      Add_Controller_Switches (Config, Controller_ID'Access, Controller_Addr'Access, Controller_Transport'Access);

      Define_Switch (Config,
                     Output      => TaskT'Access,
                     Long_Switch => "--task:",
                     Help        => "scheduled task definition",
                     Argument    => "<start>,<end>,[<weekdays>],[<segments>],<linked>");

      Getopt (Config, Concatenate => True);
      Extract_Controller_Args (Controller_ID, Controller_Addr, Controller_Transport, C);

      --  return add-task command specific args
      declare
         S       : String renames TaskT.all;
         Task_ID : Uhppoted.Lib.Task_Type;

         I : constant Natural := Ada.Strings.Fixed.Index (S, ",", 1);
         J : constant Natural := Ada.Strings.Fixed.Index (S, ",", I + 1);
         K : constant Natural := Ada.Strings.Fixed.Index (S, "[", J + 1);
         L : constant Natural := Ada.Strings.Fixed.Index (S, "],", K + 1);
         M : constant Natural := Ada.Strings.Fixed.Index (S, ",", L + 2);
         N : constant Natural := Ada.Strings.Fixed.Index (S, ",", M + 1);

         Token_1    : constant String (1 .. I - 1)     := S (1 .. I - 1);
         Start_Date : constant String (1 .. J - I - 1) := S (I + 1 .. J - 1);
         End_Date   : constant String (1 .. K - J - 1) := S (J + 1 .. K - 1);
         Weekdays   : constant String (1 .. L - K - 1) := S (K + 1 .. L - 1);
         Start_Time : constant String (1 .. M - L - 2) := S (L + 2 .. M - 1);
         Door       : constant String (1 .. N - M - 1) := S (M + 1 .. N - 1);
         More_Cards : constant String                  := S (N + 1 .. S'Last);
      begin
         if Token_1 = "control door" then
            Task_ID := Uhppoted.Lib.Door_Controlled;
         elsif Token_1 = "unlock door" then
            Task_ID := Uhppoted.Lib.Door_Normally_Open;
         elsif Token_1 = "lock door" then
            Task_ID := Uhppoted.Lib.Door_Normally_Closed;
         elsif Token_1 = "disable time profiles" then
            Task_ID := Uhppoted.Lib.Disable_Time_Profile;
         elsif Token_1 = "enable time profiles" then
            Task_ID := Enable_Time_Profile;
         elsif Token_1 = "enable card (no PIN)" then
            Task_ID := Card_No_Password;
         elsif Token_1 = "enable card IN PIN" then
            Task_ID := Card_In_Password;
         elsif Token_1 = "enable card PIN" then
            Task_ID := Card_InOut_Password;
         elsif Token_1 = "enable more cards" then
            Task_ID := Enable_More_Cards;
         elsif Token_1 = "disable more cards" then
            Task_ID := Disable_More_Cards;
         elsif Token_1 = "trigger once" then
            Task_ID := Trigger_Once;
         elsif Token_1 = "disable pushbutton" then
            Task_ID := Disable_PushButton;
         elsif Token_1 = "enable pushbutton" then
            Task_ID := Enable_PushButton;
         else
            raise Invalid_Argument with "invalid task type";
         end if;

         return (T           => ArgParse.Add_Task_Args,
                 Controller  => C,
                 Door        => 0,
                 Card        => (others => <>),
                 Event_Index => 0,
                 Profile_ID  => 0,
                 TaskT       => (Task_ID    => Task_ID,
                                 Start_Date => (Year  => Unsigned_16'Value (Start_Date (1 .. 4)),
                                                Month => Unsigned_8'Value  (Start_Date (6 .. 7)),
                                                Day   => Unsigned_8'Value  (Start_Date (9 .. 10))),
                                 End_Date   => (Year  => Unsigned_16'Value (End_Date (1 .. 4)),
                                                Month => Unsigned_8'Value  (End_Date (6 .. 7)),
                                                Day   => Unsigned_8'Value  (End_Date (9 .. 10))),
                                 Weekdays   => (Monday    => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "MON") > 0,
                                                Tuesday   => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "TUE") > 0,
                                                Wednesday => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "WED") > 0,
                                                Thursday  => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "THU") > 0,
                                                Friday    => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "FRI") > 0,
                                                Saturday  => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "SAT") > 0,
                                                Sunday    => Ada.Strings.Fixed.Index (To_Upper (Weekdays), "SUN") > 0),
                                 Start_Time => To_HHmm (Start_Time),
                                 Door       => Unsigned_8'Value (Door),
                                 More_Cards => Unsigned_8'Value (More_Cards)));
      end;

   end Parse_Add_Task;

end ArgParse;
