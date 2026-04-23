with GNAT.Sockets;
with Uhppoted.Lib.Types;

package Uhppoted.Lib.Transport.UDP is

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  Request  64 byte request packet.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          List of packets received within the timeout.
   function Broadcast (U       : UHPPOTE;
                       Request : Uhppoted.Lib.Types.Packet;
                       Timeout : Duration) return Uhppoted.Lib.Types.Packet_List;

   --  Broadcasts a 64 byte request packet to a specific controller and returns the response (if any).
   --
   --  @param  U        UHPPOTE configuration.
   --  @param  Request  64 byte request packet.
   --  @param  Timeout  Operation timeout (defaults to 2.5s).
   --
   --  @return          Received packet.
   function BroadcastTo (U       : UHPPOTE;
                         Request : Uhppoted.Lib.Types.Packet;
                         Timeout : Duration) return Uhppoted.Lib.Types.Packet;

   --  Sends a 64 byte request packet to a specific controller over 'connectedf UDP' and returns the response (if any).
   --
   --  @param  U         UHPPOTE configuration.
   --  @param  DestAddr  Controller IPv4 address:port.
   --  @param  Request   64 byte request packet.
   --  @param  Timeout   Operation timeout (defaults to 2.5s).
   --
   --  @return          Received packet.
   function SendTo      (U        : UHPPOTE;
                         DestAddr : GNAT.Sockets.Sock_Addr_Type;
                         Request  : Uhppoted.Lib.Types.Packet;
                         Timeout  : Duration) return Uhppoted.Lib.Types.Packet;

   --  Establishes a listening socket to receive controller events.
   --
   --  @param  U         UHPPOTE configuration.
   procedure Listen (U : UHPPOTE; X : Event_Handler'Class);

   --  Controlled_Type wrapper for GNAT.Socket.
   type S is tagged limited private;

private
   --  Controlled_Type wrapper for GNAT.Socket.
   type S is new Ada.Finalization.Limited_Controlled with record
      Client : Socket_Type := GNAT.Sockets.No_Socket;
   end record;

   --  Creates the wrapped socket handle.
   overriding procedure Initialize (E : in out S);

   --  Closes the wrapped socket handle.
   overriding procedure Finalize (E : in out S);

end Uhppoted.Lib.Transport.UDP;
