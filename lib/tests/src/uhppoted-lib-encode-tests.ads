with AUnit.Test_Cases;

package Uhppoted.Lib.Encode.Tests is
   type Encoder_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Encoder_Test);
   overriding function Name (T : Encoder_Test) return AUnit.Message_String;

   procedure Test_Encode_Find_Controllers (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Get_Controller (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Set_IPv4 (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Get_Time (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Set_Time (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Get_Listener (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Set_Listener (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Get_Status (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Get_Listener_Addr_Port (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Set_Listener_Addrport (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Get_Door (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Set_Door (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Set_Door_Passcodes (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Encode_Set_Door_Passcodes_With_Invalid_Passcode (T : in out AUnit.Test_Cases.Test_Case'Class);

end Uhppoted.Lib.Encode.Tests;
