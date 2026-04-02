with Interfaces;
with GNAT.Sockets;
with GNAT.Command_Line;
with GNAT.Strings;
with Uhppoted.Lib;

package ArgParse is
   use Interfaces;
   use GNAT.Command_Line;
   use GNAT.Strings;
   use GNAT.Sockets;

   type Args_Type is (
      General_Args,
      Find_Controllers_Args,
      Get_Controller_Args,
      Set_IPv4_Args,
      Get_Time_Args,
      Set_Time_Args,
      Get_Listener_Args,
      Set_Listener_Args,
      Get_Status_Args,
      Get_Door_Args,
      Set_Door_Args,
      Set_Door_Passcodes_Args,
      Open_Door_Args,
      Get_Card_Args);

   type Args (T : Args_Type) is record
      Controller : Uhppoted.Lib.Controller;
      Door       : Unsigned_8;
      Card       : Unsigned_32;

      case T is
         when Set_IPv4_Args =>
            Address : Inet_Addr_Type;
            Netmask : Inet_Addr_Type;
            Gateway : Inet_Addr_Type;

         when Set_Listener_Args =>
            Listener : Sock_Addr_Type;
            Interval : Unsigned_8;

         when Get_Door_Args =>
            null;

         when Set_Door_Args =>
            Mode      : Uhppoted.Lib.Control_Mode;
            OpenDelay : Unsigned_8;

         when Set_Door_Passcodes_Args =>
            Passcodes : Uhppoted.Lib.Passcodes_List (1 .. 4);

         when Open_Door_Args =>
            null;

         when Get_Card_Args =>
            null;

         when others =>
            null;
      end case;
   end record;

   function Parse (Cmd : String) return Args;

private
   procedure Add_Controller_Switches (Config               : in out Command_Line_Configuration;
                                      Controller_ID        : access Integer;
                                      Controller_Addr      : access String_Access;
                                      Controller_Transport : access String_Access);

   procedure Extract_Controller_Args (Controller_ID        : Integer;
                                      Controller_Addr      : String_Access;
                                      Controller_Transport : String_Access;
                                      C                    : out Uhppoted.Lib.Controller);

   function Parse_Set_IPv4 return Args;
   function Parse_Set_Listener return Args;
   function Parse_Get_Door return Args;
   function Parse_Set_Door return Args;
   function Parse_Set_Door_Passcodes return Args;
   function Parse_Open_Door return Args;
   function Parse_Get_Card return Args;

end ArgParse;
