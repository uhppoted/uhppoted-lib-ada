with GNAT.Sockets;

package Uhppoted.Lib.Integration_Tests.Stub is
   procedure ListenUDP (Socket : GNAT.Sockets.Socket_Type; Port : GNAT.Sockets.Port_Type);
   procedure ListenTCP (Socket : GNAT.Sockets.Socket_Type; Port : GNAT.Sockets.Port_Type);
end Uhppoted.Lib.Integration_Tests.Stub;
