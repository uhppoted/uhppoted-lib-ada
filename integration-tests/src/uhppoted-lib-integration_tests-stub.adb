with Ada.Text_IO;
with Ada.Streams;
with Ada.Calendar;
with Ada.Unchecked_Conversion;

with Uhppoted.Lib.Integration_Tests.Stub.Messages;

package body Uhppoted.Lib.Integration_Tests.Stub is
   use GNAT.Sockets;
   use Ada.Calendar;

   UDP  : Socket_Type;

   subtype Packet is Ada.Streams.Stream_Element_Array (1 .. 64);

   function To_Stream is new Ada.Unchecked_Conversion (Source => Messages.Reply, Target =>  Packet);
   function From_Packet is new Ada.Unchecked_Conversion (Source => Packet,  Target => Messages.Request);

   procedure Listen (Port : Port_Type) is
      Bind : Sock_Addr_Type;

      Read_Set : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Selector : Selector_Type;
      Status : Selector_Status;

      Deadline : Time := Clock + 2.5;
   begin
      Bind.Addr := Any_Inet_Addr;
      Bind.Port := Port;

      Create_Socket (UDP, Family_Inet, Socket_Datagram);
      Bind_Socket (UDP, Bind);

      Create_Selector (Selector);
      Empty (Read_Set);
      Empty (Write_Set);
      Set (Read_Set, UDP);

      loop
         declare
            Remaining  : Duration;

            Buffer : Packet;
            Offset : Ada.Streams.Stream_Element_Offset;
            From : Sock_Addr_Type;
            Replies : Messages.Reply_List;

         begin
            --  NTS: github workflow hangs indefinitely without a CheckSelector
            Remaining := Deadline - Clock;

            exit when Remaining <= 0.0;

            Check_Selector (Selector,
                            R_Socket_Set => Read_Set,
                            W_Socket_Set => Write_Set,
                            Status       => Status,
                            Timeout      => 5.0);

            if Status = Completed then
               Receive_Socket (UDP, Buffer, Offset, From);
               Replies := Messages.Get (From_Packet (Buffer));
               for Reply of Replies loop
                  Send_Socket (UDP, To_Stream (Reply), Offset, From);
               end loop;

               --  Reset close timeout
               Deadline := Clock + 1.0;
            end if;
         end;
      end loop;

      Close_Selector (Selector);
      Close_Socket (UDP);
   end Listen;

   procedure Stop is
   begin
      Close_Socket (UDP);
   end Stop;

end Uhppoted.Lib.Integration_Tests.Stub;
