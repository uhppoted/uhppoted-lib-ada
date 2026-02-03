with Interfaces;

package Uhppoted.Lib is
   type Packet is array (1 .. 64) of Interfaces.Unsigned_8;

   type CID is new Interfaces.Unsigned_32;
   type IPv4 is array (1 .. 4) of Interfaces.Unsigned_8;
   subtype MAC_Address is String (1 .. 17);
   subtype Version is String (1 .. 4);

   type DateOnly is record
      Year  : Interfaces.Unsigned_16;
      Month : Interfaces.Unsigned_8;
      Day   : Interfaces.Unsigned_8;
   end record;

   type Controller is record
      ID       : CID;
      Address  : IPv4;
      Gateway  : IPv4;
      Netmask  : IPv4;
      MAC      : MAC_Address;
      Firmware : Version;
      Date     : DateOnly;
   end record;

   type Controller_List is array (Positive range <>) of Controller;

   --  Retrieves a list of controllers available on the local LAN.
   function Find_Controllers return Controller_List;

   function To_String (Addr : IPv4) return String;
   function To_String (Date : DateOnly) return String;
end Uhppoted.Lib;
