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
         Date     => (
            Year  => 2018,
            Month => 11,
            Day   => 5));

   Set_IPv4 : constant Boolean := True;

   Get_Time : constant DateTime := (Year => 2025, Month => 11, Day => 1, Hour => 12, Minute => 34, Second => 56);
   Set_Time : constant DateTime := (Year => 2025, Month => 11, Day => 1, Hour => 12, Minute => 34, Second => 56);

end Uhppoted.Lib.Integration_Tests.Expected;
