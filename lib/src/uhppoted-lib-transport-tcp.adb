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
      BindAddr : constant Sock_Addr_Type := U.Bind_Addr;
      Offset   : Stream_Element_Offset;

      Sock   : S;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 64);
      Reply  : Packet;

      Selector  : H;
      Read_Set  : Socket_Set_Type;
      Write_Set : Socket_Set_Type;
      Status    : Selector_Status;
      Deadline  : constant Time := Clock + Timeout;

   begin
      --  validate
      if BindAddr = DestAddr then
         raise Invalid_Address_Error;
      end if;

      --  non-blocking connect
      Bind_Socket (Sock.Client, BindAddr);

      declare
         Non_Blocking : Request_Type := (Name => Non_Blocking_IO, Enabled => True);
         Blocking     : Request_Type := (Name => Non_Blocking_IO, Enabled => False);
         Err          : Error_Type;
         Remaining    : Duration;
      begin
         begin
            Control_Socket (Sock.Client, Non_Blocking);
            Connect_Socket (Sock.Client, DestAddr);
         exception
            when E : Socket_Error =>
               Err := Resolve_Exception (E);
               if Err /= Operation_Now_In_Progress and then Err /= Resource_Temporarily_Unavailable then
                  raise;
               end if;
         end;

         Remaining := Deadline - Clock;
         Empty (Read_Set);
         Empty (Write_Set);
         Set (Write_Set, Sock.Client);

         Check_Selector (Selector.Selector,
                         R_Socket_Set => Read_Set,
                         W_Socket_Set => Write_Set,
                         Status       => Status,
                         Timeout      => Remaining);

         if Status = Completed and then Is_Set (Write_Set, Sock.Client) then
            declare
               Err : constant Option_Type := Get_Socket_Option (Sock.Client, Socket_Level, Error);
            begin
               if Err.Error /= Success then
                  raise Socket_Error with "Connect failed: " & Err.Error'Image;
               end if;
            end;

            Control_Socket (Sock.Client, Blocking);
         else
            raise Timeout_Error;
         end if;
      end;

      --  send/receive
      Send_Socket (Sock.Client, To_Stream (Request), Offset);
      if U.Debug then
         Dump ("... sent to " & Image (DestAddr) & " (TCP)", Request);
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
                  Receive_Socket (Sock.Client, Buffer, Offset);
                  if Offset = 64 then
                     Reply := To_Packet (Buffer);

                     if U.Debug then
                        Dump ("... received from " & Image (DestAddr) & " (TCP)", Reply);
                     end if;

                     return Reply;
                  end if;

               when Expired | Aborted =>
                  raise Timeout_Error;
            end case;
         end;
      end loop;
   end Send;

end Uhppoted.Lib.Transport.TCP;
