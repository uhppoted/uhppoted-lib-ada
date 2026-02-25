with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Calendar;

package body Uhppoted.Lib.Transport.TCP is
   use GNAT.Sockets;
   use Ada.Calendar;

   subtype Stream_Packet is Ada.Streams.Stream_Element_Array (1 .. 64);
   Timeout_Error : exception;

   function To_Stream is new Ada.Unchecked_Conversion (Source => Packet, Target =>  Stream_Packet);
   function To_Packet is new Ada.Unchecked_Conversion (Source => Stream_Packet,  Target => Packet);

   --  Sends a 64 byte request packet to a specific IPv4 address:port and returns the response (if any).
   function Send (U        : UHPPOTE;
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
      Create_Socket (Client);

      begin
         Bind_Socket (Client, Bind);
         Connect_Socket (Client, DestAddr);

         Empty (Read_Set);
         Empty (Write_Set);
         Set (Read_Set, Client);

         Send_Socket (Client, To_Stream (Request), Offset);

         if Request (2) = 16#96# then
            Close_Selector (Selector);
            Close_Socket (Client);

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
   end Send;

end Uhppoted.Lib.Transport.TCP;
