with AUnit.Test_Cases;

package Uhppoted.Lib.Integration_Tests.Default is
   use AUnit.Test_Cases;

   type Integration_Test is new AUnit.Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Integration_Test);
   overriding function  Name           (T : Integration_Test) return AUnit.Message_String;
   overriding procedure Set_Up_Case    (T : in out Integration_Test);
   overriding procedure Tear_Down_Case (T : in out Integration_Test);
   overriding procedure Set_Up         (T : in out Integration_Test);
   overriding procedure Tear_Down      (T : in out Integration_Test);

private
   task Listen;

   procedure Test_Find_Controllers    (T : in out Test_Case'Class);
   procedure Test_Get_Controller      (T : in out Test_Case'Class);
   procedure Test_Set_IPv4            (T : in out Test_Case'Class);
   procedure Test_Get_Time            (T : in out Test_Case'Class);
   procedure Test_Set_Time            (T : in out Test_Case'Class);
   procedure Test_Get_Listener        (T : in out Test_Case'Class);
   procedure Test_Set_Listener        (T : in out Test_Case'Class);
   procedure Test_Get_Status          (T : in out Test_Case'Class);
   procedure Test_Get_Status_No_Event (T : in out Test_Case'Class);
   procedure Test_Get_Door            (T : in out Test_Case'Class);
   procedure Test_Set_Door            (T : in out Test_Case'Class);
   procedure Test_Set_Door_Passcodes  (T : in out Test_Case'Class);
   procedure Test_Open_Door           (T : in out Test_Case'Class);
   procedure Test_Get_Cards           (T : in out Test_Case'Class);
   procedure Test_Get_Card            (T : in out Test_Case'Class);
   procedure Test_Get_Card_Not_Found  (T : in out Test_Case'Class);
   procedure Test_Get_Card_At_Index   (T : in out Test_Case'Class);
   procedure Test_Get_Card_At_Index_Not_Found (T : in out Test_Case'Class);
   procedure Test_Get_Card_At_Index_Deleted (T : in out Test_Case'Class);
   procedure Test_Put_Card            (T : in out Test_Case'Class);
   procedure Test_Delete_Card         (T : in out Test_Case'Class);
   procedure Test_Delete_All_Cards    (T : in out Test_Case'Class);
   procedure Test_Get_Event_Index     (T : in out Test_Case'Class);
   procedure Test_Set_Event_Index     (T : in out Test_Case'Class);

end Uhppoted.Lib.Integration_Tests.Default;
