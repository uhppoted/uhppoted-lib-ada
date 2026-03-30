with Ada.Streams;
with Ada.Calendar;

package body Uhppoted.Lib.Transport.UDP is
   use Ada.Streams;
   use Ada.Calendar;
   use Uhppoted.Lib.Types;

   --  Creates the wrapped socket handle.
   overriding procedure Initialize (E : in out S) is
   begin
      Create_Socket (E.Client, Family_Inet, Socket_Datagram);
   end Initialize;

   --  Closes the wrapped socket handle.
   overriding procedure Finalize (E : in out S) is
   begin
      if E.Client /= No_Socket then
         Close_Socket (E.Client);
         E.Client := No_Socket;
      end if;
   end Finalize;

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   function Broadcast (U : UHPPOTE; Request : Packet; Timeout : Duration) return Packet_List is
      BindAddr  : constant Sock_Addr_Type := U.Bind_Addr;
      DestAddr  : constant Sock_Addr_Type := U.Broadcast_Addr;
      Offset    : Stream_Element_Offset;

      Sock    : S;
      From    : Sock_Addr_Type;
      Buffer  : Stream_Element_Array (1 .. 64);
      Replies : Packet_List;

      Selector  : H;
      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Status    : Selector_Status;
      Deadline  : constant Time := Clock + Timeout;
   begin
      Replies.Reserve_Capacity (16);

      Bind_Socket (Sock.Client, BindAddr);
      Set_Socket_Option (Sock.Client, Socket_Level, (Broadcast, True));

      Send_Socket (Sock.Client, To_Stream (Request), Offset, DestAddr);
      if U.Debug then
         Dump ("... sent to " & Image (DestAddr) & " (UDP)", Request);
      end if;

      loop
         declare
            Now       : constant Time := Clock;
            Remaining : Duration;
            Reply     : Packet;
         begin
            Remaining := Deadline - Now;

            exit when Remaining <= 0.0;

            Empty (Read_Set);
            Empty (Write_Set);
            Set (Read_Set, Sock.Client);

            Check_Selector (Selector.Selector,
                            R_Socket_Set => Read_Set,
                            W_Socket_Set => Write_Set,
                            Status       => Status,
                            Timeout      => Remaining);

            if Status = Completed then
               Receive_Socket (Sock.Client, Buffer, Offset, From);

               if Offset = 64 then
                  Reply := To_Packet (Buffer);
                  Replies.Append (Reply);

                  if U.Debug then
                     Dump ("... received from " & Image (From) & " (UDP)", Reply);
                  end if;
               end if;

            elsif Status = Expired then
               exit;
            end if;
         end;
      end loop;

      return Replies;
   end Broadcast;

   --  Broadcasts a 64 byte request packet to a specific controller and returns the response (if any).
   function BroadcastTo (U        : UHPPOTE;
                         Request  : Packet;
                         Timeout  : Duration) return Packet is
      BindAddr  : constant Sock_Addr_Type := U.Bind_Addr;
      Offset    : Stream_Element_Offset;

      Sock   : S;
      From   : Sock_Addr_Type;
      Buffer : Stream_Element_Array (1 .. 64);
      Reply  : Packet;

      Selector  : H;
      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Status    : Selector_Status;
      Deadline  : constant Time := Clock + Timeout;

   begin
      Bind_Socket (Sock.Client, BindAddr);
      Set_Socket_Option (Sock.Client, Socket_Level, (Broadcast, True));

      Send_Socket (Sock.Client, To_Stream (Request), Offset, U.Broadcast_Addr);
      if U.Debug then
         Dump ("... sent to " & Image (U.Broadcast_Addr) & " (UDP)", Request);
      end if;

      if Request (2) = 16#96# then
         Reply := [
            Request (1), Request (2), Request (3), Request (4),
            Request (5), Request (6), Request (7), Request (8),
            16#01#,
            others => 16#00#
         ];

         return Reply;
      end if;

      loop
         declare
            Remaining : Duration;

         begin
            Remaining := Deadline - Clock;

            Empty (Read_Set);
            Empty (Write_Set);
            Set (Read_Set, Sock.Client);

            if Remaining <= 0.0 then
               raise Timeout_Error;
            end if;

            Check_Selector (Selector.Selector,
                            R_Socket_Set => Read_Set,
                            W_Socket_Set => Write_Set,
                            Status       => Status,
                            Timeout      => Remaining);

            case Status is
               when Completed =>
                  Receive_Socket (Sock.Client, Buffer, Offset, From);

                  if Offset = 64 then
                     Reply := To_Packet (Buffer);
                     if U.Debug then
                        Dump ("... received from " & Image (From) & " (UDP)", Reply);
                     end if;

                     return Reply;
                  end if;

               when Expired | Aborted =>
                  raise Timeout_Error;
            end case;
         end;
      end loop;

   end BroadcastTo;

   --  Sends a 64 byte request packet to a specific controller over 'connectedf UDP' and returns the response (if any).
   function SendTo (U        : UHPPOTE;
                    DestAddr : Sock_Addr_Type;
                    Request  : Packet;
                    Timeout  : Duration) return Packet is
      BindAddr  : constant Sock_Addr_Type := U.Bind_Addr;
      Offset    : Stream_Element_Offset;

      Sock   : S;
      From   : Sock_Addr_Type;
      Buffer : Stream_Element_Array (1 .. 64);
      Reply  : Packet;

      Selector  : H;
      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Status    : Selector_Status;
      Deadline  : constant Time := Clock + Timeout;

   begin
      Bind_Socket (Sock.Client, BindAddr);
      Connect_Socket (Sock.Client, DestAddr);

      Send_Socket (Sock.Client, To_Stream (Request), Offset);
      if U.Debug then
         Dump ("... sent to " & Image (DestAddr) & " (UDP)", Request);
      end if;

      if Request (2) = 16#96# then
         Reply := [
            Request (1), Request (2), Request (3), Request (4),
            Request (5), Request (6), Request (7), Request (8),
            16#01#,
            others => 16#00#
         ];

         return Reply;
      end if;

      loop
         declare
            Remaining : Duration;

         begin
            Remaining := Deadline - Clock;

            if Remaining <= 0.0 then
               raise Timeout_Error;
            end if;

            Empty (Read_Set);
            Empty (Write_Set);
            Set (Read_Set, Sock.Client);

            Check_Selector (Selector.Selector,
                            R_Socket_Set => Read_Set,
                            W_Socket_Set => Write_Set,
                            Status       => Status,
                            Timeout      => Remaining);

            case Status is
               when Completed =>
                  Receive_Socket (Sock.Client, Buffer, Offset, From);

                  if Offset = 64 then
                     Reply := To_Packet (Buffer);

                     if U.Debug then
                        Dump ("... received from " & Image (From) & " (UDP)", Reply);
                     end if;

                     return Reply;
                  end if;

               when Expired | Aborted =>
                  raise Timeout_Error;
            end case;
         end;
      end loop;

   end SendTo;

end Uhppoted.Lib.Transport.UDP;
