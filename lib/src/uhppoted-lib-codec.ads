--  Message constant definitions.
--
package Uhppoted.Lib.Codec is

   --  Start of message byte.
   SOM : Unsigned_8  := 16#17#;

   --  Start of message byte for v6.62 listener events.
   SOM_v6_62 : Unsigned_8  := 16#19#;

   --  Authorisation value.
   MagicWord : Unsigned_32 := 16#55aaaa55#;

   --  Message op-codes.
   --
   --  @enum  Get_Status                  Op-code for get-status message.
   --  @enum  Set_Time                    Op-code for set-time message.
   --  @enum  Get_Time                    Op-code for get-time message.
   --  @enum  Open_Door                   Op-code for open-door message.
   --  @enum  Put_Card                    Op-code for put-card message.
   --  @enum  Delete_Card                 Op-code for delete-card message.
   --  @enum  Delete_All_Cards            Op-code for delete-all-cards message.
   --  @enum  Get_Cards                   Op-code for get-cards message.
   --  @enum  Get_Card                    Op-code for get-card message.
   --  @enum  Get_Card_At_Index           Op-code for get-card-at-index message.
   --  @enum  Set_Door                    Op-code for set-door message.
   --  @enum  Get_Door                    Op-code for get-door message.
   --  @enum  Set_Antipassback            Op-code for set-antipassback message.
   --  @enum  Get_Antipassback            Op-code for get-antipassback message.
   --  @enum  Set_Time_Profile            Op-code for set-time-profile message.
   --  @enum  Clear_Time_Profiles         Op-code for clear-time-profiles message.
   --  @enum  Set_Door_Passcodes          Op-code for set-door-passcodes message.
   --  @enum  Record_Special_Events       Op-code for record-special-events message.
   --  @enum  Set_Listener                Op-code for set-listener message.
   --  @enum  Get_Listener                Op-code for get-listener message.
   --  @enum  Get_Controller              Op-code for get-controller message.
   --  @enum  Set_IPv4                    Op-code for set-ipv4 message.
   --  @enum  Get_Time_Profile            Op-code for get-time-profile message.
   --  @enum  Set_PC_Control              Op-code for set-pc-control message.
   --  @enum  Set_Interlock               Op-code for set-interlock message.
   --  @enum  Activate_Keypads            Op-code for activate-keypads message.
   --  @enum  Clear_Task_List             Op-code for clear-task-list message.
   --  @enum  Add_Task                    Op-code for add-task message.
   --  @enum  Set_First_Card              Op-code for set-first-card message.
   --  @enum  Refresh_Task_List           Op-code for refresh-task-list message.
   --  @enum  Get_Event                   Op-code for get-event message.
   --  @enum  Set_Event_Index             Op-code for set-event-index message.
   --  @enum  Get_Event_Index             Op-code for get-event-index message.
   --  @enum  Restore_Default_Parameters  Op-code for restore-default-parameters message.
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
      Set_Antipassback,
      Get_Antipassback,
      Set_Time_Profile,
      Clear_Time_Profiles,
      Set_Door_Passcodes,
      Record_Special_Events,
      Set_Listener,
      Get_Listener,
      Get_Controller,
      Set_IPv4,
      Get_Time_Profile,
      Set_PC_Control,
      Set_Interlock,
      Activate_Keypads,
      Clear_Task_List,
      Add_Task,
      Set_First_Card,
      Refresh_Task_List,
      Get_Event,
      Set_Event_Index,
      Get_Event_Index,
      Restore_Default_Parameters);

   for Op_Code use (
      Get_Status                 => 16#20#,
      Set_Time                   => 16#30#,
      Get_Time                   => 16#32#,
      Open_Door                  => 16#40#,
      Put_Card                   => 16#50#,
      Delete_Card                => 16#52#,
      Delete_All_Cards           => 16#54#,
      Get_Cards                  => 16#58#,
      Get_Card                   => 16#5a#,
      Get_Card_At_Index          => 16#5c#,
      Set_Door                   => 16#80#,
      Get_Door                   => 16#82#,
      Set_Antipassback           => 16#84#,
      Get_Antipassback           => 16#86#,
      Set_Time_Profile           => 16#88#,
      Clear_Time_Profiles        => 16#8a#,
      Set_Door_Passcodes         => 16#8c#,
      Record_Special_Events      => 16#8e#,
      Set_Listener               => 16#90#,
      Get_Listener               => 16#92#,
      Get_Controller             => 16#94#,
      Set_IPv4                   => 16#96#,
      Get_Time_Profile           => 16#98#,
      Set_PC_Control             => 16#a0#,
      Set_Interlock              => 16#a2#,
      Activate_Keypads           => 16#a4#,
      Clear_Task_List            => 16#a6#,
      Add_Task                   => 16#a8#,
      Set_First_Card             => 16#aa#,
      Refresh_Task_List          => 16#ac#,
      Get_Event                  => 16#b0#,
      Set_Event_Index            => 16#b2#,
      Get_Event_Index            => 16#b4#,
      Restore_Default_Parameters => 16#c8#
   );

end Uhppoted.Lib.Codec;
