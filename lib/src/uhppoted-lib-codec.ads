package Uhppoted.Lib.Codec is

   SOM : Unsigned_8 := 16#17#;

   type Op_Code is (
      Get_Status,
      Set_Time,
      Get_Time,
      Set_Door,
      Get_Door,
      Set_Door_Passcodes,
      Set_Listener,
      Get_Listener,
      Get_Controller,
      Set_IPv4);

   for Op_Code use (
      Get_Status         => 16#20#,
      Set_Time           => 16#30#,
      Get_Time           => 16#32#,
      Set_Door           => 16#80#,
      Get_Door           => 16#82#,
      Set_Door_Passcodes => 16#8c#,
      Set_Listener       => 16#90#,
      Get_Listener       => 16#92#,
      Get_Controller     => 16#94#,
      Set_IPv4           => 16#96#
   );

end Uhppoted.Lib.Codec;
