with AUnit.Assertions;
with GNAT.Sockets;
with Ada.Streams;

package body Uhppoted.Lib.Integration_Tests is
   use AUnit.Assertions;
   use GNAT.Sockets;

   UDP  : Socket_Type;
   U : constant UHPPOTE := (
      Bind_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 0),

      Broadcast_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("192.168.1.255"),
         Port => 60005),

      Listen_Addr => (
         Family => GNAT.Sockets.Family_Inet,
         Addr => Inet_Addr ("0.0.0.0"),
         Port => 60001),

      Debug => True);

   overriding function Name (T : Integration_Test) return AUnit.Message_String is
   begin
      return AUnit.Format ("integration tests");
   end Name;

   overriding procedure Register_Tests (T : in out Integration_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Find_Controllers'Access, "Test Find_Controllers");
   end Register_Tests;

   overriding procedure Set_Up (T : in out Integration_Test) is
   begin
      null;
   end Set_Up;

   overriding procedure Tear_Down (T : in out Integration_Test) is
   begin
      Close_Socket (UDP);
   end Tear_Down;

   task body Listen is
      Bind : Sock_Addr_Type;

      subtype Packet is Ada.Streams.Stream_Element_Array (1 .. 64);
   begin
      Bind.Addr := Any_Inet_Addr;
      Bind.Port := 60005;

      Create_Socket (UDP, Family_Inet, Socket_Datagram);
      Bind_Socket (UDP, Bind);

      loop
         declare
            Buffer : Packet;
            Offset : Ada.Streams.Stream_Element_Offset;
            From : Sock_Addr_Type;

            Replies : constant array (1 .. 3) of Packet := [
               1 => [
                  16#17#, 16#94#, 16#00#, 16#00#, 16#78#, 16#37#, 16#2a#, 16#18#,
                  16#c0#, 16#a8#, 16#01#, 16#64#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
                  16#c0#, 16#a8#, 16#01#, 16#01#, 16#00#, 16#12#, 16#23#, 16#34#,
                  16#45#, 16#56#, 16#08#, 16#92#, 16#20#, 16#18#, 16#11#, 16#05#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
               ],

               2 => [
                  16#17#, 16#94#, 16#00#, 16#00#, 16#41#, 16#78#, 16#1e#, 16#12#,
                  16#c0#, 16#a8#, 16#01#, 16#64#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
                  16#c0#, 16#a8#, 16#01#, 16#01#, 16#52#, 16#fd#, 16#fc#, 16#07#,
                  16#21#, 16#82#, 16#08#, 16#92#, 16#20#, 16#19#, 16#08#, 16#15#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
               ],

               3 => [
                  16#17#, 16#94#, 16#00#, 16#00#, 16#90#, 16#53#, 16#fb#, 16#0b#,
                  16#c0#, 16#a8#, 16#01#, 16#65#, 16#ff#, 16#ff#, 16#ff#, 16#00#,
                  16#c0#, 16#a8#, 16#01#, 16#01#, 16#52#, 16#fd#, 16#fc#, 16#07#,
                  16#21#, 16#82#, 16#06#, 16#62#, 16#20#, 16#20#, 16#01#, 16#01#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                  16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#
               ]
           ];

         begin
            Receive_Socket (UDP, Buffer, Offset, From);
            for Reply of Replies loop
               Send_Socket (UDP, Reply, Offset, From);
            end loop;
         end;
      end loop;

   end Listen;

   procedure Test_Find_Controllers (T : in out Test_Case'Class) is
      pragma Unreferenced (T);

      C405419896 : constant Uhppoted.Lib.Controller := (
         ID       => 405419896,
         Address  => [192, 168, 1, 100],
         Netmask  => [255, 255, 255, 0],
         Gateway  => [192, 168, 1, 1],
         MAC      => [16#00#, 16#12#, 16#23#, 16#34#, 16#45#, 16#56#],
         Firmware => "0892",
         Date     => (
            Year  => 2018,
            Month => 11,
            Day   => 5));

      C303986753 : constant Uhppoted.Lib.Controller := (
         ID       => 303986753,
         Address  => [192, 168, 1, 100],
         Netmask  => [255, 255, 255, 0],
         Gateway  => [192, 168, 1, 1],
         MAC      => [16#52#, 16#fd#, 16#fc#, 16#07#, 16#21#, 16#82#],
         Firmware => "0892",
         Date     => (
            Year  => 2019,
            Month => 8,
            Day   => 15));

      C201020304 : constant Uhppoted.Lib.Controller := (
         ID       => 201020304,
         Address  => [192, 168, 1, 101],
         Netmask  => [255, 255, 255, 0],
         Gateway  => [192, 168, 1, 1],
         MAC      => [16#52#, 16#fd#, 16#fc#, 16#07#, 16#21#, 16#82#],
         Firmware => "0662",
         Date     => (
            Year  => 2020,
            Month => 1,
            Day   => 1));

      Controllers : constant Controller_List := Find_Controllers (U);
   begin
      Assert (Controllers'Length = 3, "expected 3 controllers, got" & Controllers'Length'Image);
      Assert (Controllers (1) = C405419896, "invalid 405419896 controller record");
      Assert (Controllers (2) = C303986753, "invalid 303986753 controller record");
      Assert (Controllers (3) = C201020304, "invalid 201020304 controller record");

   end Test_Find_Controllers;

end Uhppoted.Lib.Integration_Tests;
