with Ada.Finalization;
with GNAT.Sockets;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Transport.UDP is
   function Broadcast (U       : UHPPOTE;
                       Request : Uhppoted.Lib.Types.Packet;
                       Timeout : Duration) return Uhppoted.Lib.Types.Packet_List;

   function BroadcastTo (U       : UHPPOTE;
                         Request : Uhppoted.Lib.Types.Packet;
                         Timeout : Duration) return Uhppoted.Lib.Types.Packet;

   function SendTo      (U        : UHPPOTE;
                         DestAddr : GNAT.Sockets.Sock_Addr_Type;
                         Request  : Uhppoted.Lib.Types.Packet;
                         Timeout  : Duration) return Uhppoted.Lib.Types.Packet;

   type S is tagged limited private;

private
   type S is new Ada.Finalization.Limited_Controlled with
   record
      Client : Socket_Type := GNAT.Sockets.No_Socket;
   end record;

   overriding
   procedure Initialize (E : in out S);

   overriding
   procedure Finalize (E : in out S);

end Uhppoted.Lib.Transport.UDP;
