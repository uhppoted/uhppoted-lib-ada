with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Calendar;

package body Uhppoted.Lib.UDP is
   use GNAT.Sockets;
   use Ada.Calendar;

   subtype Stream_Packet is Ada.Streams.Stream_Element_Array (1 .. 64);
   Timeout_Error : exception;

   function To_Stream is new Ada.Unchecked_Conversion (Source => Packet, Target =>  Stream_Packet);
   function To_Packet is new Ada.Unchecked_Conversion (Source => Stream_Packet,  Target => Packet);

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   function Broadcast (U : UHPPOTE; Request : Packet; Timeout : Duration) return Packet_List is
      Client    : Socket_Type;
      Bind      : constant Sock_Addr_Type := U.Bind_Addr;
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
   begin
      Replies.Reserve_Capacity (16);

      Create_Selector (Selector);
      Empty (Read_Set);
      Empty (Write_Set);

      Create_Socket (Client, Family_Inet, Socket_Datagram);
      Bind_Socket (Client, Bind);
      Set_Socket_Option (Client, Socket_Level, (Broadcast, True));
      Set (Read_Set, Client);

      Send_Socket (Client, To_Stream (Request), Offset, DestAddr);

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
               Receive_Socket (Client, Buffer, Offset, From);
               Reply := To_Packet (Buffer);
               Replies.Append (Reply);

            elsif Status = Expired then
               exit;
            end if;
         end;
      end loop;

      Close_Selector (Selector);
      Close_Socket (Client);

      return Replies;
   end Broadcast;

   --  Sends a 64 byte request packet to a specific IPv4 address:port and returns the response (if any).
   function SendTo (U        : UHPPOTE;
                    DestAddr : Sock_Addr_Type;
                    Request  : Packet;
                    Timeout  : Duration) return Packet is
      Client    : Socket_Type;
      Bind      : constant Sock_Addr_Type := U.Bind_Addr;
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

   begin
      Create_Selector (Selector);
      Create_Socket (Client, Family_Inet, Socket_Datagram);

      begin
         Bind_Socket (Client, Bind);
         Set_Socket_Option (Client, Socket_Level, (Broadcast, True));

         Empty (Read_Set);
         Empty (Write_Set);
         Set (Read_Set, Client);

         Send_Socket (Client, To_Stream (Request), Offset, DestAddr);

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
               Receive_Socket (Client, Buffer, Offset, From);
               Reply := To_Packet (Buffer);

            when Expired | Aborted =>
               raise Timeout_Error;
         end case;

         Close_Selector (Selector);
         Close_Socket (Client);

         return Reply;

      exception
         when others =>
            Close_Selector (Selector);
            Close_Socket (Client);
            raise;
      end;
   end SendTo;

end Uhppoted.Lib.UDP;
