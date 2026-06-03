with Ada.Strings.Unbounded;
with Uhppoted.Lib.Types;
with Uhppoted.Lib.Responses;
with Uhppoted.Lib.Replies;

package Uhppoted.Lib.Decode is

   --  Decodes a 64 byte get-controller reply as a Get_Controller_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Controller (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Controller_Response;

   --  Decodes a 64 byte set-IPv4 reply as a Set_IPv4_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_IPv4 (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_IPv4_Response;

   --  Decodes a 64 byte get-time reply as a Get_Time_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Time (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Time_Response;

   --  Decodes a 64 byte set-time reply as a Set_Time_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Time (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Time_Response;

   --  Decodes a 64 byte get-listener reply as a Get_Listener_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Listener (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Listener_Response;

   --  Decodes a 64 byte get-listener reply as a Get_Listener_Addr_Port_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Listener_Addr_Port (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Listener_Addr_Port_Response;

   --  Decodes a 64 byte set-listener reply as a Set_Listener_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Listener (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Listener_Response;

   --  Decodes a 64 byte set-listener reply as a Set_Listener_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Listener_Addr_Port (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Listener_Addr_Port_Response;

   --  Decodes a 64 byte get-status reply as a Get_Status_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Status (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Status_Response;

   --  Decodes a 64 byte get-door reply as a Get_Door_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Door (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Door_Response;

   --  Decodes a 64 byte set-door reply as a Get_Door_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Door (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Door_Response;

   --  Decodes a 64 byte set-door-passcodes reply as a Get_Door_Passcodes_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Door_Passcodes (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Door_Passcodes_Response;

   --  Decodes a 64 byte open-door reply as an Open_Door_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Open_Door (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Open_Door_Response;

   --  Decodes a 64 byte get-cards reply as a Get_Cards_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Cards (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Cards_Response;

   --  Decodes a 64 byte get-card reply as a Get_Card_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Card (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Card_Response;

   --  Decodes a 64 byte get-card reply as a Get_Card_At_Index_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Card_At_Index (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Card_At_Index_Response;

   --  Decodes a 64 byte put-card reply as a Put_Card_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Put_Card (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Put_Card_Response;

   --  Decodes a 64 byte delete-card reply as a Delete_Card_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Delete_Card (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Delete_Card_Response;

   --  Decodes a 64 byte delete-all-cards reply as a Delete_All_Cards_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Delete_All_Cards (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Delete_All_Cards_Response;

   --  Decodes a 64 byte get-event reply as a Get_Event_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Event (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Event_Response;

   --  Decodes a 64 byte get-event-index reply as a Get_Event_Index_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Event_Index (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Event_Index_Response;

   --  Decodes a 64 byte set-event-index reply as a Set_Event_Index_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Event_Index (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Event_Index_Response;

   --  Decodes a 64 byte record-special-events reply as a Record_Special_Events_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Record_Special_Events (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Record_Special_Events_Response;

   --  Decodes a 64 byte get-time-profile reply as a Get_Time_Profile_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Time_Profile (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Time_Profile_Response;

   --  Decodes a 64 byte set-time-profile reply as a Set_Time_Profile_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Time_Profile (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Time_Profile_Response;

   --  Decodes a 64 byte clear-time-profiles reply as a Clear_Time_Profiles_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Clear_Time_Profiles (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Clear_Time_Profiles_Response;

   --  Decodes a 64 byte add-task reply as an Add_Task_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Add_Task (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Add_Task_Response;

   --  Decodes a 64 byte refresh-tasklist reply as a Refresh_Task_List_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Refresh_Task_List (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Refresh_Task_List_Response;

   --  Decodes a 64 byte clear-tasklist reply as a Clear_Task_List_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Clear_Task_List (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Clear_Task_List_Response;

   --  Decodes a 64 byte set-pc-control reply as a Set_PC_Control_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_PC_Control (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_PC_Control_Response;

   --  Decodes a 64 byte set-interlock reply as a Set_Interlock_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Interlock (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Interlock_Response;

   --  Decodes a 64 byte activate-keypads reply as a Activate_Keypads_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Activate_Keypads (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Activate_Keypads_Response;

   --  Decodes a 64 byte get-antipassback reply as a Get_Antipassback_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Get_Antipassback (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Antipassback_Response;

   --  Decodes a 64 byte set-antipassback reply as a Set_Antipassback_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_Antipassback (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Antipassback_Response;

   --  Decodes a 64 byte set-firstcard reply as a Set_First_Card_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Set_First_Card (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_First_Card_Response;

   --  Decodes a 64 byte restore-default-parameters reply as a Restore_Default_Parameters_Response record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Restore_Default_Parameters (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Restore_Default_Parameters_Response;

   --  Decodes a 64 byte listener-event packet as a Listener_Event record.
   --
   --  @param  Reply  64 byte reply packet to decode.
   function Listener_Event (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Listener_Event;

private
   function BCD_To_String (Bytes : Uhppoted.Lib.Types.BCD) return String;
   --  Translates a BCD coded string in a byte array to a string.

   function Unpack_Version (V : Uhppoted.Lib.Replies.Version_Field) return Ada.Strings.Unbounded.Unbounded_String;
   --  Translates a BCD coded version to a vN.NN formatted string.

   function Unpack_Boolean (B : Unsigned_8) return Boolean;
   --  Translates an Unsigned_8 into a Boolean.

   function Unpack_Date (Bytes : Uhppoted.Lib.Types.BCD) return DateOnly;
   --  Translates a BCD coded date to a DateOnly.

   function Unpack_Short_Date (Bytes : Uhppoted.Lib.Types.BCD) return DateOnly;
   --  Translates a BCD coded YYMMDD date to a DateOnly.

   function Unpack_Time (Bytes : Uhppoted.Lib.Types.BCD) return TimeOnly;
   --  Translates a BCD coded time to a TimeOnly.

   function Unpack_Date_Time (Bytes : Uhppoted.Lib.Types.BCD) return DateTime;
   --  Translates a BCD coded date/time to a DateTime.

   function Unpack_HHmm (Bytes : Uhppoted.Lib.Types.BCD) return HHmm;
   --  Translates a BCD coded time to an HHmm.

end Uhppoted.Lib.Decode;
