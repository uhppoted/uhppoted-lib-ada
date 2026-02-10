with GNAT.Sockets;
with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Calendar;

package body Uhppoted.Lib.UDP is
   use GNAT.Sockets;
   use Ada.Calendar;

   subtype Stream_Packet is Ada.Streams.Stream_Element_Array (1 .. 64);

   function To_Stream is new Ada.Unchecked_Conversion (Source => Packet, Target =>  Stream_Packet);
   function To_Packet is new Ada.Unchecked_Conversion (Source => Stream_Packet,  Target => Packet);

   --  Broadcasts a 64 byte request packet and returns the response (if any).
   function Broadcast (Request : Packet) return Packet_List is
      Client  : Socket_Type;
      Bind    : Sock_Addr_Type;
      Address : Sock_Addr_Type;
      Offset  : Ada.Streams.Stream_Element_Offset;

      Read_Set : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Selector : Selector_Type;
      Status : Selector_Status;
      Start_Time : constant Time := Clock;
      Deadline : constant Time := Start_Time + 2.5;

      From     : Sock_Addr_Type;
      Buffer   : Ada.Streams.Stream_Element_Array (1 .. 64);
      Replies  : Packet_List;
   begin
      Replies.Reserve_Capacity (16);

      Bind.Addr := Any_Inet_Addr;
      Bind.Port := 0;

      Address.Addr := Inet_Addr ("192.168.1.255");
      Address.Port := 60000;

      Create_Selector (Selector);

      Empty (Read_Set);
      Empty (Write_Set);

      Create_Socket (Client, Family_Inet, Socket_Datagram);
      Bind_Socket (Client, Bind);
      Set_Socket_Option (Client, Socket_Level, (Broadcast, True));
      Set (Read_Set, Client);

      Send_Socket (Client, To_Stream (Request), Offset, Address);

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

end Uhppoted.Lib.UDP;
