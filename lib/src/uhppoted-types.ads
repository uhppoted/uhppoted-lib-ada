with Interfaces;
with Ada.Strings.Unbounded;

package Uhppoted.Types is
   use Ada.Strings.Unbounded;
   use Interfaces;

   type IPv4 is array (1 .. 4) of Interfaces.Unsigned_8;
   type Hardware_Addr is array (1 .. 6) of Unsigned_8;
   subtype Firmware_Version is String (1 .. 5);

   type DateOnly is record
      Year  : Unsigned_16;
      Month : Unsigned_8;
      Day   : Unsigned_8;
   end record;

   type DateTime is record
      Year   : Unsigned_16;
      Month  : Unsigned_8;
      Day    : Unsigned_8;
      Hour   : Unsigned_8;
      Minute : Unsigned_8;
      Second : Unsigned_8;
   end record;

   type Controller_Record is record
      ID       : Unsigned_32;
      Address  : IPv4;
      Netmask  : IPv4;
      Gateway  : IPv4;
      MAC      : Hardware_Addr;
      Firmware : Unbounded_String;
      Date     : DateOnly;
   end record;

   type Controller_Record_List is array (Positive range <>) of Controller_Record;

   function Image (Addr : IPv4) return String;
   function Image (MAC  : Hardware_Addr) return String;
   function Image (D    : DateOnly) return String;
   function Image (DT   : DateTime) return String;

end Uhppoted.Types;
