package Uhppoted.Lib.Codec is

   SOM       : Unsigned_8  := 16#17#;
   MagicWord : Unsigned_32 := 16#55aaaa55#;

   type Op_Code is (
      Get_Status,
      Set_Time,
      Get_Time,
      Open_Door,
      Put_Card,
      Delete_Card,
      Delete_All_Cards,
      Get_Cards,
      Get_Card,
      Get_Card_At_Index,
      Set_Door,
      Get_Door,
      Set_Door_Passcodes,
      Record_Special_Events,
      Set_Listener,
      Get_Listener,
      Get_Controller,
      Set_IPv4,
      Set_Event_Index,
      Get_Event_Index);

   for Op_Code use (
      Get_Status            => 16#20#,
      Set_Time              => 16#30#,
      Get_Time              => 16#32#,
      Open_Door             => 16#40#,
      Put_Card              => 16#50#,
      Delete_Card           => 16#52#,
      Delete_All_Cards      => 16#54#,
      Get_Cards             => 16#58#,
      Get_Card              => 16#5a#,
      Get_Card_At_Index     => 16#5c#,
      Set_Door              => 16#80#,
      Get_Door              => 16#82#,
      Set_Door_Passcodes    => 16#8c#,
      Record_Special_Events => 16#8e#,
      Set_Listener          => 16#90#,
      Get_Listener          => 16#92#,
      Get_Controller        => 16#94#,
      Set_IPv4              => 16#96#,
      Set_Event_Index       => 16#b2#,
      Get_Event_Index       => 16#b4#
   );

end Uhppoted.Lib.Codec;
