with Interfaces;
with GNAT.Sockets;
with Uhppoted.Lib;

package ArgParse is
   use Interfaces;

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
      Get_Door_Args);

   type Args (T : Args_Type) is
   record
      Controller : Uhppoted.Lib.Controller;

      case T is
         when Set_Listener_Args =>
            Listener : GNAT.Sockets.Sock_Addr_Type;
            Interval : Unsigned_8;

         when Get_Door_Args =>
            Door : Unsigned_8;

         when others =>
            null;
      end case;
   end record;

   function Parse (Cmd : String) return Args;

private
   function Parse_Set_Listener  return Args;
   function Parse_Get_Door     return Args;

end ArgParse;
