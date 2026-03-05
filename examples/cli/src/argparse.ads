with Interfaces;
with Uhppoted.Lib;

package ArgParse is
   type Args_Type is (
      Find_Controllers_Args,
      Get_Controller_Args,
      Set_IPv4_Args,
      Get_Time_Args,
      Set_Time_Args);

   type Args (T : Args_Type) is
   record
      Controller : Uhppoted.Lib.Controller;
   end record;

   function Parse (Cmd : String) return Args;

end ArgParse;
