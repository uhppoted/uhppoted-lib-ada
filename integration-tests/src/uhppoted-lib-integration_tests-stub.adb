with Ada.Streams;
with Ada.Calendar;
with Ada.Unchecked_Conversion;

with Uhppoted.Lib.Integration_Tests.Stub.Messages;

package body Uhppoted.Lib.Integration_Tests.Stub is
   use GNAT.Sockets;
   use Ada.Calendar;

   subtype Packet is Ada.Streams.Stream_Element_Array (1 .. 64);

   function To_Stream is new Ada.Unchecked_Conversion (Source => Messages.Reply, Target =>  Packet);
   function From_Packet is new Ada.Unchecked_Conversion (Source => Packet,  Target => Messages.Request);

   procedure ListenUDP (Port : Port_Type) is
      Socket   : Socket_Type;
      Bind     : Sock_Addr_Type;
      Deadline : Time := Clock + 2.5;

      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Selector  : Selector_Type;
      Status    : Selector_Status;
   begin
      Bind.Addr := Any_Inet_Addr;
      Bind.Port := Port;

      Create_Socket (Socket, Family_Inet, Socket_Datagram);
      Bind_Socket (Socket, Bind);

      Create_Selector (Selector);
      Empty (Read_Set);
      Empty (Write_Set);
      Set (Read_Set, Socket);

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
               Receive_Socket (Socket, Buffer, Offset, From);
               Replies := Messages.Get (From_Packet (Buffer));
               for Reply of Replies loop
                  Send_Socket (Socket, To_Stream (Reply), Offset, From);
               end loop;

               --  Reset close timeout
               Deadline := Clock + 1.0;
            end if;
         end;
      end loop;

      Close_Selector (Selector);
      Close_Socket (Socket);
   end ListenUDP;

   procedure ListenTCP (Port : Port_Type) is
      Socket   : Socket_Type;
      Bind     : Sock_Addr_Type;
      Deadline : Time := Clock + 2.5;

      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Selector  : Selector_Type;
      Status    : Selector_Status;
   begin
      Bind.Addr := Any_Inet_Addr;
      Bind.Port := Port;

      Create_Socket (Socket);
      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Socket, Bind);
      Listen_Socket (Socket);

      Create_Selector (Selector);
      Empty (Read_Set);
      Empty (Write_Set);
      Set (Read_Set, Socket);

      loop
         declare
            Remaining : Duration;
            Client    : Socket_Type;

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
               if Is_Set (Read_Set, Socket) then
                  declare
                     Addr : Sock_Addr_Type;
                  begin
                     Accept_Socket (Socket, Client, Addr);
                     Set (Read_Set, Client);
                  end;
               end if;
               
               if Client /= No_Socket and then Is_Set (Read_Set, Client) then
                  declare
                     Buffer  : Packet;
                     Offset  : Ada.Streams.Stream_Element_Offset;
                     Request : Packet;
                     Replies : Messages.Reply_List;
                  begin
                     Receive_Socket (Client, Buffer, Offset);

                     Request := Buffer (Buffer'First .. Offset);
                     Replies := Messages.Get (From_Packet (Request));
                     for Reply of Replies loop
                        Send_Socket (Client, To_Stream (Reply), Offset);
                     end loop;
                  end;
               end if;

               Deadline := Clock + 1.0;
            end if;
         end;
      end loop;

      Close_Selector (Selector);
      Close_Socket (Socket);
   end ListenTCP;

end Uhppoted.Lib.Integration_Tests.Stub;
