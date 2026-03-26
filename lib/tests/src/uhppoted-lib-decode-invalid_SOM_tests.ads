with AUnit.Test_Cases;

package Uhppoted.Lib.Decode.Invalid_SOM_Tests is
   type SOM_Test is new AUnit.Test_Cases.Test_Case with null record;
   type SOM_Test_Access is access all SOM_Test;

   overriding function  Name           (T : SOM_Test) return AUnit.Message_String;
   overriding procedure Register_Tests (T : in out SOM_Test);

   procedure Test_Get_Controller_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_IPv4_Invalid_SOM       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Time_Invalid_SOM       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Time_Invalid_SOM       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Listener_Invalid_SOM   (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Listener_Invalid_SOM   (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Status_Invalid_SOM     (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Listener_Address_Port_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Listener_Address_Port_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Door_Invalid_SOM       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Door_Invalid_SOM       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Door_Passcodes_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);

end Uhppoted.Lib.Decode.Invalid_SOM_Tests;
