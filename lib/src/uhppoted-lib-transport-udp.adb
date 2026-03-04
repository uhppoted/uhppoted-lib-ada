with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Calendar;

package body Uhppoted.Lib.Transport.UDP is
   use Ada.Calendar;
   use Uhppoted.Lib.Types;

   subtype Stream_Packet is Ada.Streams.Stream_Element_Array (1 .. 64);
   Timeout_Error : exception;

   function To_Stream is new Ada.Unchecked_Conversion (Source => Packet, Target =>  Stream_Packet);
   function To_Packet is new Ada.Unchecked_Conversion (Source => Stream_Packet,  Target => Packet);

   --  Creates the wrapped socket handle.
   overriding procedure Initialize (E : in out S) is
   begin
      Create_Socket (E.Client, Family_Inet, Socket_Datagram);
   end Initialize;

   --  Closes the wrapped socket handle.
   overriding procedure Finalize (E : in out S) is
   begin
      if E.Client /= GNAT.Sockets.No_Socket then
         Close_Socket (E.Client);
         E.Client := No_Socket;
      end if;
   end Finalize;

   --  Binds a socket to the bind address and enables SO_BROADCAST.
   procedure Bind (E         : in out S;
                   Addr      : Sock_Addr_Type;
                   Broadcast : Boolean) is
   begin
      Bind_Socket (E.Client, Addr);
      if Broadcast then
         Set_Socket_Option (E.Client, Socket_Level, (GNAT.Sockets.Broadcast, True));
      end if;
   end Bind;

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   function Broadcast (U : UHPPOTE; Request : Packet; Timeout : Duration) return Packet_List is
      BindAddr  : constant Sock_Addr_Type := U.Bind_Addr;
      DestAddr  : constant Sock_Addr_Type := U.Broadcast_Addr;
      Offset    : Ada.Streams.Stream_Element_Offset;

      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Selector  : Selector_Type;
      Status    : Selector_Status;
      Deadline  : constant Time := Clock + Timeout;

      From      : Sock_Addr_Type;
      Buffer    : Ada.Streams.Stream_Element_Array (1 .. 64);
      Replies   : Packet_List;

      Sock : S;
   begin
      Replies.Reserve_Capacity (16);

      Create_Selector (Selector);
      Empty (Read_Set);
      Empty (Write_Set);

      Bind (Sock, BindAddr, True);
      Set (Read_Set, Sock.Client);

      Send_Socket (Sock.Client, To_Stream (Request), Offset, DestAddr);

      loop
         declare
            Now : constant Time := Clock;
            Remaining  : Duration;
            Reply : Packet;
         begin
            Remaining := Deadline - Now;

            exit when Remaining <= 0.0;

            Check_Selector (Selector,
                            R_Socket_Set => Read_Set,
                            W_Socket_Set => Write_Set,
                            Status       => Status,
                            Timeout      => Remaining);

            if Status = Completed then
               Receive_Socket (Sock.Client, Buffer, Offset, From);
               Reply := To_Packet (Buffer);
               Replies.Append (Reply);

            elsif Status = Expired then
               exit;
            end if;
         end;
      end loop;

      Close_Selector (Selector);

      return Replies;
   end Broadcast;

   --  Sends a 64 byte request packet to a specific controller over UDP broadcast and returns the response (if any).
   function BroadcastTo (U        : UHPPOTE;
                         Request  : Packet;
                         Timeout  : Duration) return Packet is
      BindAddr  : constant Sock_Addr_Type := U.Bind_Addr;
      Offset    : Ada.Streams.Stream_Element_Offset;

      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Selector  : Selector_Type;
      Status    : Selector_Status;
      Deadline  : constant Time := Clock + Timeout;
      Remaining : constant Duration := Deadline - Clock;

      From   : Sock_Addr_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 64);
      Reply  : Packet;

      Sock : S;
   begin
      Create_Selector (Selector);

      begin
         Bind (Sock, BindAddr, True);

         Empty (Read_Set);
         Empty (Write_Set);
         Set (Read_Set, Sock.Client);

         Send_Socket (Sock.Client, To_Stream (Request), Offset, U.Broadcast_Addr);

         if Request (2) = 16#96# then
            Close_Selector (Selector);

            Reply := [
               Request (1), Request (2), Request (3), Request (4),
               Request (5), Request (6), Request (7), Request (8),
               16#01#,
               others => 16#00#
            ];

            return Reply;
         end if;

         if Remaining <= 0.0 then
            raise Timeout_Error;
         end if;

         Check_Selector (Selector,
                         R_Socket_Set => Read_Set,
                         W_Socket_Set => Write_Set,
                         Status       => Status,
                         Timeout      => Remaining);

         case Status is
            when Completed =>
               Receive_Socket (Sock.Client, Buffer, Offset, From);
               Reply := To_Packet (Buffer);

            when Expired | Aborted =>
               raise Timeout_Error;
         end case;

         Close_Selector (Selector);

         return Reply;

      exception
         when others =>
            Close_Selector (Selector);
            raise;
      end;
   end BroadcastTo;

   --  Sends a 64 byte request packet to a specific IPv4 address:port and returns the response (if any).
   function SendTo (U        : UHPPOTE;
                    DestAddr : Sock_Addr_Type;
                    Request  : Packet;
                    Timeout  : Duration) return Packet is
      BindAddr  : constant Sock_Addr_Type := U.Bind_Addr;
      Offset    : Ada.Streams.Stream_Element_Offset;

      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Selector  : Selector_Type;
      Status    : Selector_Status;
      Deadline  : constant Time := Clock + Timeout;
      Remaining : constant Duration := Deadline - Clock;

      From   : Sock_Addr_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 64);
      Reply  : Packet;

      Sock : S;
   begin
      Create_Selector (Selector);

      begin
         Bind (Sock, BindAddr, False);

         Empty (Read_Set);
         Empty (Write_Set);
         Set (Read_Set, Sock.Client);

         Send_Socket (Sock.Client, To_Stream (Request), Offset, DestAddr);

         if Request (2) = 16#96# then
            Close_Selector (Selector);

            Reply := [
               Request (1), Request (2), Request (3), Request (4),
               Request (5), Request (6), Request (7), Request (8),
               16#01#,
               others => 16#00#
            ];

            return Reply;
         end if;

         if Remaining <= 0.0 then
            raise Timeout_Error;
         end if;

         Check_Selector (Selector,
                         R_Socket_Set => Read_Set,
                         W_Socket_Set => Write_Set,
                         Status       => Status,
                         Timeout      => Remaining);

         case Status is
            when Completed =>
               Receive_Socket (Sock.Client, Buffer, Offset, From);
               Reply := To_Packet (Buffer);

            when Expired | Aborted =>
               raise Timeout_Error;
         end case;

         Close_Selector (Selector);

         return Reply;

      exception
         when others =>
            Close_Selector (Selector);
            raise;
      end;
   end SendTo;

end Uhppoted.Lib.Transport.UDP;
