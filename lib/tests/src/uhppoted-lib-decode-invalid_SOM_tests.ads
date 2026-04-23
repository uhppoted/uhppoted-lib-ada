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
   procedure Test_Open_Door_Invalid_SOM      (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Cards_Invalid_SOM      (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Card_Invalid_SOM       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Card_Not_Found_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Card_At_Index_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Put_Card_Invalid_SOM       (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Delete_Card_Invalid_SOM    (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Delete_All_Cards_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Event_Invalid_SOM      (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Event_Not_Found_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Event_Overwritten_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Event_Index_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Set_Event_Index_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Record_Special_Events_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Listener_Event_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Listener_Event_V6_62_Invalid_SOM (T : in out AUnit.Test_Cases.Test_Case'Class);

end Uhppoted.Lib.Decode.Invalid_SOM_Tests;
