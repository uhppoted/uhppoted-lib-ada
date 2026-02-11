with Uhppoted.Types;

package Uhppoted.Lib is
   use Uhppoted.Types;

   subtype Controller      is Uhppoted.Types.Controller;
   subtype Controller_List is Uhppoted.Types.Controller_List;

   function Find_Controllers return Controller_List;

   function Image (Addr : IPv4) return String renames Uhppoted.Types.Image;
   function Image (MAC : MAC_Address) return String renames Uhppoted.Types.Image;
   function Image (Date : DateOnly) return String renames Uhppoted.Types.Image;

end Uhppoted.Lib;
