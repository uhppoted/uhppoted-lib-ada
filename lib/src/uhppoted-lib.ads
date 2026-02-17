with Interfaces;
with Uhppoted.Types;

package Uhppoted.Lib is
   use Interfaces;
   use Uhppoted.Types;

   subtype UHPPOTE         is Uhppoted.Types.UHPPOTE;
   subtype Controller      is Uhppoted.Types.Controller;
   subtype Controller_List is Uhppoted.Types.Controller_List;

   function Find_Controllers (U : UHPPOTE) return Controller_List;
   function Get_Controller   (U : UHPPOTE; C : Unsigned_32) return Controller;

   function Image (Addr : IPv4) return String renames Uhppoted.Types.Image;
   function Image (MAC : MAC_Address) return String renames Uhppoted.Types.Image;
   function Image (Date : DateOnly) return String renames Uhppoted.Types.Image;

end Uhppoted.Lib;
