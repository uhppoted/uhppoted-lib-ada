with GNAT.Sockets;

package Uhppoted.Lib.Integration_Tests.Stub is
   procedure ListenUDP (Port : GNAT.Sockets.Port_Type);
   procedure ListenTCP (Port : GNAT.Sockets.Port_Type);
end Uhppoted.Lib.Integration_Tests.Stub;
