with Ada.Streams;
with Ada.Calendar;

package body Uhppoted.Lib.Transport.TCP is
   use Ada.Streams;
   use Ada.Calendar;

   --  Creates the wrapped socket handle.
   overriding
   procedure Initialize (E : in out S) is
   begin
      Create_Socket (E.Client, Family_Inet);
   end Initialize;

   --  Closes the wrapped socket handle.
   overriding
   procedure Finalize (E : in out S) is
   begin
      if E.Client /= No_Socket then
         Close_Socket (E.Client);
         E.Client := No_Socket;
      end if;
   end Finalize;

   --  Creates the wrapped list of sockets.
   overriding
   procedure Initialize (E : in out Socket_List) is
   begin
      Create_Socket (E.Clients (1), Family_Inet);
   end Initialize;

   --  Closes the wrapped socket handle.
   overriding
   procedure Finalize (E : in out Socket_List) is
   begin
      for I in 1 .. 5 loop
         if E.Clients (I) /= No_Socket then
            Close_Socket (E.Clients (I));
            E.Clients (I) := No_Socket;
         end if;
      end loop;
   end Finalize;

   --  Returns either the active socket (if any) or No_Socket.
   function Client (E : Socket_List) return Socket_Type is
   begin
      if E.IX <= 5 then
         return E.Clients (E.IX);
      end if;

      return No_Socket;
   end Client;

   --  Creates a new socket and adds it to the list of managed sockets.
   procedure Next (E : in out Socket_List) is
   begin
      E.IX := E.IX + 1;
      Create_Socket (E.Clients (E.IX), Family_Inet, Socket_Datagram);
   end Next;

   --  Sends a 64 byte request packet to a specific IPv4 address:port and returns the response (if any).
   function Send (U : UHPPOTE; DestAddr : Sock_Addr_Type; Request : Packet; Timeout : Duration) return Packet is
      BindAddr : constant Sock_Addr_Type := U.Bind_Addr;
      Offset   : Stream_Element_Offset;

      Sockets : Socket_List;
      Buffer  : Ada.Streams.Stream_Element_Array (1 .. 64);
      Reply   : Packet;

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

      --  send-to-self
      --
      --  If the bind port is 0, retries (up to 5 times) until the OS binds to a port that is not the
      --  destination port. The interim sockets are retained to prevent the OS from reusing the invalid
      --  port, which it will happily do if the socket is closed immediately.
      --
      --  It should really match on both address and port but multiple network cards, etc make it complicated
      --  and fragile.
      declare
         Endpoint : Sock_Addr_Type;
         Retries  : Positive := 1;
      begin
         loop
            Bind_Socket (Sockets.Client, BindAddr);

            Endpoint := Get_Socket_Name (Sockets.Client);

            exit when Endpoint.Port /= DestAddr.Port;

            if BindAddr.Port /= 0 then
               raise Bind_Address_Port_Reserved_For_Destination;
            end if;

            if Retries >= 5 then
               raise Bound_Address_Port_Reserved_For_Destination;
            end if;

            Sockets.Next;
            Retries := Retries + 1;
         end loop;
      end;

      --  non-blocking connect
      declare
         Non_Blocking : Request_Type := (Name => Non_Blocking_IO, Enabled => True);
         Blocking     : Request_Type := (Name => Non_Blocking_IO, Enabled => False);
         Err          : Error_Type;
         Remaining    : Duration;
      begin
         begin
            Control_Socket (Sockets.Client, Non_Blocking);
            Connect_Socket (Sockets.Client, DestAddr);
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
         Set (Write_Set, Sockets.Client);

         Check_Selector
           (Selector.Selector,
            R_Socket_Set => Read_Set,
            W_Socket_Set => Write_Set,
            Status       => Status,
            Timeout      => Remaining);

         if Status = Completed and then Is_Set (Write_Set, Sockets.Client) then
            declare
               Err : constant Option_Type := Get_Socket_Option (Sockets.Client, Socket_Level, Error);
            begin
               if Err.Error /= Success then
                  raise Socket_Error with "Connect failed: " & Err.Error'Image;
               end if;
            end;

            Control_Socket (Sockets.Client, Blocking);
         else
            raise Timeout_Error;
         end if;
      end;

      --  send/receive
      Send_Socket (Sockets.Client, To_Stream (Request), Offset);
      if U.Debug then
         Dump ("... sent to " & Image (DestAddr) & " (TCP)", Request);
      end if;

      if Request (2) = 16#96# then
         Reply :=
           [Request (1),
            Request (2),
            Request (3),
            Request (4),
            Request (5),
            Request (6),
            Request (7),
            Request (8),
            16#01#,
            others => 16#00#];

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
            Set (Read_Set, Sockets.Client);

            Check_Selector
              (Selector.Selector,
               R_Socket_Set => Read_Set,
               W_Socket_Set => Write_Set,
               Status       => Status,
               Timeout      => Remaining);

            case Status is
               when Completed         =>
                  Receive_Socket (Sockets.Client, Buffer, Offset);
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
