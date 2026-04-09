with Uhppoted.Types;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Encode is

   --  Encodes a get-controller request as a 64 byte array.
   function Get_Controller (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-IPv4 request as a 64 byte array.
   function Set_IPv4 (Controller : Unsigned_32;
                      Addr       : GNAT.Sockets.Inet_Addr_Type;
                      Netmask    : GNAT.Sockets.Inet_Addr_Type;
                      Gateway    : GNAT.Sockets.Inet_Addr_Type) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-time request as a 64 byte array.
   function Get_Time (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-time request as a 64 byte array.
   function Set_Time (Controller : Unsigned_32;
                      DT         : Uhppoted.Types.DateTime) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-listener request as a 64 byte array.
   function Get_Listener_Addr_Port (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-listener request as a 64 byte array.
   function Set_Listener (Controller : Unsigned_32;
                          Addr       : GNAT.Sockets.Inet_Addr_Type;
                          Port       : Unsigned_16;
                          Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-listener request as a 64 byte array.
   function Set_Listener_Addr_Port (Controller : Unsigned_32;
                                    Listener   : GNAT.Sockets.Sock_Addr_Type;
                                    Interval   : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-status request as a 64 byte array.
   function Get_Status (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-door request as a 64 byte array.
   function Get_Door (Controller : Unsigned_32; Door : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-door request as a 64 byte array.
   function Set_Door (Controller : Unsigned_32;
                      Door       : Unsigned_8;
                      Mode       : Uhppoted.Lib.Control_Mode;
                      OpenDelay  : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a set-door-passcodes request as a 64 byte array.
   function Set_Door_Passcodes (Controller : Unsigned_32;
                                Door       : Unsigned_8;
                                Passcode1  : Unsigned_32;
                                Passcode2  : Unsigned_32;
                                Passcode3  : Unsigned_32;
                                Passcode4  : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes an open-door request as a 64 byte array.
   function Open_Door (Controller : Unsigned_32;
                       Door       : Unsigned_8) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-cards request as a 64 byte array.
   function Get_Cards (Controller : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-card request as a 64 byte array.
   function Get_Card (Controller : Unsigned_32; Card : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a get-card-by-index request as a 64 byte array.
   function Get_Card_At_Index (Controller : Unsigned_32; Index : Unsigned_32) return Uhppoted.Lib.Types.Packet;

   --  Encodes a put-card request as a 64 byte array.
   function Put_Card (Controller : Unsigned_32;
                      Card       : Unsigned_32;
                      Start_Date : DateOnly;
                      End_Date   : DateOnly;
                      Door_1     : Unsigned_8;
                      Door_2     : Unsigned_8;
                      Door_3     : Unsigned_8;
                      Door_4     : Unsigned_8;
                      PIN        : Unsigned_24) return Uhppoted.Lib.Types.Packet;

end Uhppoted.Lib.Encode;
