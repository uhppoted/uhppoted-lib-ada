package Uhppoted.Lib.Codec is

   SOM : Unsigned_8 := 16#17#;

   type Op_Code is (
      Set_Time, 
      Get_Time, 
      Get_Controller, 
      Set_IPv4);

   for Op_Code use (
      Set_Time       => 16#30#,
      Get_Time       => 16#32#,
      Get_Controller => 16#94#,
      Set_IPv4       => 16#96#
   );

end Uhppoted.Lib.Codec;
