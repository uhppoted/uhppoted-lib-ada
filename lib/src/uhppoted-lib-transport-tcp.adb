with Ada.Streams;
with Ada.Calendar;

package body Uhppoted.Lib.Transport.TCP is
   use Ada.Streams;
   use Ada.Calendar;

   --  Creates the wrapped socket handle.
   overriding procedure Initialize (E : in out S) is
   begin
      Create_Socket (E.Client, Family_Inet);
   end Initialize;

   --  Closes the wrapped socket handle.
   overriding procedure Finalize (E : in out S) is
   begin
      if E.Client /= No_Socket then
         Close_Socket (E.Client);
         E.Client := No_Socket;
      end if;
   end Finalize;

   --  Sends a 64 byte request packet to a specific IPv4 address:port and returns the response (if any).
   function Send (U        : UHPPOTE;
                  DestAddr : Sock_Addr_Type;
                  Request  : Packet;
                  Timeout  : Duration) return Packet is
      Bind   : constant Sock_Addr_Type := U.Bind_Addr;
      Offset : Stream_Element_Offset;

      Sock   : S;
      From   : Sock_Addr_Type (Family_Inet);
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 64);
      Reply  : Packet;

      Selector  : H;
      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Status    : Selector_Status;
      Deadline  : constant Time := Clock + Timeout;
      Remaining : constant Duration := Deadline - Clock;

   begin
      Bind_Socket    (Sock.Client, Bind);
      Connect_Socket (Sock.Client, DestAddr);

      Empty (Read_Set);
      Empty (Write_Set);
      Set   (Read_Set, Sock.Client);

      Send_Socket (Sock.Client, To_Stream (Request), Offset);

      if Request (2) = 16#96# then
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

      Check_Selector (Selector.Selector,
                      R_Socket_Set => Read_Set,
                      W_Socket_Set => Write_Set,
                      Status       => Status,
                      Timeout      => Remaining);

      case Status is
         when Completed =>
            Receive_Socket (Sock.Client, Buffer, Offset, From);
            Reply := To_Packet (Buffer);

            if U.Debug then
               Dump (DestAddr, Reply, Uhppoted.Lib.TCP);
            end if;

         when Expired | Aborted =>
            raise Timeout_Error;
      end case;

      return Reply;
   end Send;

end Uhppoted.Lib.Transport.TCP;
