with Ada.Strings.Unbounded;

package Uhppoted.Lib.Integration_Tests.Expected is
   use Ada.Strings.Unbounded;

   Find_Controllers : constant Controller_Record_List := [
      (
        ID       => 201020304,
        Address  => [192, 168, 1, 101],
        Netmask  => [255, 255, 255, 0],
        Gateway  => [192, 168, 1, 1],
        MAC      => [16#52#, 16#fd#, 16#fc#, 16#07#, 16#21#, 16#82#],
        Firmware => To_Unbounded_String ("v6.62"),
        Date     => (Year => 2020, Month => 1, Day => 1)),
      (
        ID       => 303986753,
        Address  => [192, 168, 1, 100],
        Netmask  => [255, 255, 255, 0],
        Gateway  => [192, 168, 1, 1],
        MAC      => [16#52#, 16#fd#, 16#fc#, 16#07#, 16#21#, 16#82#],
        Firmware => To_Unbounded_String ("v8.92"),
        Date     => (Year => 2019, Month => 8, Day => 15)),
      (
        ID       => 405419896,
        Address  => [192, 168, 1, 100],
        Netmask  => [255, 255, 255, 0],
        Gateway  => [192, 168, 1, 1],
        MAC      => [16#00#, 16#12#, 16#23#, 16#34#, 16#45#, 16#56#],
        Firmware => To_Unbounded_String ("v8.92"),
        Date     => (Year => 2018, Month => 11, Day => 5))
     ];

   Get_Controller : constant Controller_Record := (
      ID       => 405419896,
      Address  => [192, 168, 1, 100],
      Netmask  => [255, 255, 255, 0],
      Gateway  => [192, 168, 1, 1],
      MAC      => [16#00#, 16#12#, 16#23#, 16#34#, 16#45#, 16#56#],
      Firmware => To_Unbounded_String ("v8.92"),
      Date     => (Year => 2018, Month => 11, Day => 5));

   Set_IPv4 : constant Boolean := True;

   Get_Time : constant DateTime := (Year => 2025, Month => 11, Day => 1, Hour => 12, Minute => 34, Second => 56);

   Set_Time : constant DateTime := (Year => 2025, Month => 11, Day => 1, Hour => 12, Minute => 34, Second => 56);

   Get_Listener : constant Listener_Record := (
     AddrPort => Network_Socket_Address (Addr => Inet_Addr ("192.168.1.100"), Port => Port_Type (60001)),
     Interval => 13);

   Set_Listener : constant Boolean := True;

   Get_Status : constant Controller_Status := (
     System_Date_Time => (Year => 2022, Month => 8, Day => 23, Hour => 9, Minute => 49, Second => 39),
     Doors => [1 => (Open     => False,
                     Button   => False,
                     Unlocked => True),
               2 => (Open     => True,
                     Button   => False,
                     Unlocked => True),
               3 => (Open     => False,
                     Button   => False,
                     Unlocked => True),
               4 => (Open     => False,
                     Button   => True,
                     Unlocked => False)],
     Alarms => (Flags       => 9,
                Fire        => True,
                Lock_Forced => False),
     System_Error => 3,
     Special_Info => 39,
     Event => (Index          => 78,
               Event          => 2,
               Timestamp      => (Year => 2022, Month => 8, Day => 23, Hour => 9, Minute => 47, Second => 6),
               Door           => 3,
               Direction      => 1,
               Card           => 8165537,
               Access_Granted => True,
               Reason         => 44));

   Get_Status_No_Event : constant Controller_Status := (
     System_Date_Time => (Year => 2025, Month => 11, Day => 23, Hour => 14, Minute => 37, Second => 53),
     Doors => [1 => (Open     => True,
                     Button   => True,
                     Unlocked => True),
               2 => (Open     => False,
                     Button   => True,
                     Unlocked => True),
               3 => (Open     => True,
                     Button   => False,
                     Unlocked => True),
               4 => (Open     => True,
                     Button   => True,
                     Unlocked => False)],
     Alarms => (Flags       => 9,
                Fire        => True,
                Lock_Forced => False),
     System_Error => 27,
     Special_Info => 39,
     Event => (Index          => 0,
               Event          => 0,
               Timestamp      => (Year => 0, Month => 0, Day => 0, Hour => 0, Minute => 0, Second => 0),
               Door           => 0,
               Direction      => 0,
               Card           => 0,
               Access_Granted => False,
               Reason         => 0));

   Get_Door : constant Door_Record := (
     Mode      => To_Control_Mode (3),
     OpenDelay => 7);

end Uhppoted.Lib.Integration_Tests.Expected;
