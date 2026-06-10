with Ada.Strings;
with Ada.Strings.Fixed;
with Uhppoted.Lib.Codec;

package body Uhppoted.Lib.Decode is
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   use Uhppoted.Lib.Types;
   use Uhppoted.Lib.Codec;
   use Uhppoted.Lib.Replies;
   use Uhppoted.Lib.Responses;

   --  Decodes a 64 byte get-controller reply as a Get_Controller_Response record.
   function Get_Controller (Reply : Packet) return Responses.Get_Controller_Response is
      --!format off
      R : Replies.Get_Controller_Reply with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Controller'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return
        (Controller  => R.Controller,
         IP_Address  => R.Address,
         Subnet_Mask => R.Netmask,
         Gateway     => R.Gateway,
         MAC_Address => R.MAC,
         Version     => Unpack_Version (R.Version),
         Date        => Unpack_Date (R.Date));
   end Get_Controller;

   --  Decodes a 64 byte set-IPv4 reply as a Set_IPv4_Response record.
   function Set_IPv4 (Reply : Packet) return Responses.Set_IPv4_Response is
      --!format off
      R : Replies.Set_IPv4_Reply with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_IPv4'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Set_IPv4;

   --  Decodes a 64 byte get-time reply as a Get_Time_Response record.
   function Get_Time (Reply : Packet) return Responses.Get_Time_Response is
      --!format off
      R : Replies.Get_Time_Response with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Time'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Date_Time => Unpack_Date_Time (R.Date_Time));
   end Get_Time;

   --  Decodes a 64 byte set-time reply as a Set_Time_Response record.
   function Set_Time (Reply : Packet) return Responses.Set_Time_Response is
      --!format off
      R : Replies.Set_Time_Response with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Time'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Date_Time => Unpack_Date_Time (R.Date_Time));
   end Set_Time;

   --  Decodes a 64 byte get-listener reply as a Get_Listener_Response record.
   function Get_Listener (Reply : Packet) return Responses.Get_Listener_Response is
      --!format off
      R : Replies.Get_Listener_Response with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Listener'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Address => R.Address, Port => R.Port, Interval => R.Interval);
   end Get_Listener;

   --  Decodes a 64 byte get-listener reply as a Get_Listener_Addr_Port_Response record.
   function Get_Listener_Addr_Port (Reply : Packet) return Responses.Get_Listener_Addr_Port_Response is
      R : Replies.Get_Listener_Addr_Port_Response with Import, Address => Reply'Address;
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Listener'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return
        (Controller => R.Controller,
         Listener   => Network_Socket_Address (Addr => Inet_Addr (Image (R.Address)), Port => Port_Type (R.Port)),
         Interval   => R.Interval);
   end Get_Listener_Addr_Port;

   --  Decodes a 64 byte set-listener reply as a Set_Listener_Response record.
   function Set_Listener (Reply : Packet) return Responses.Set_Listener_Response is
      --!format off
      R : Replies.Set_Listener_Response with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Listener'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => R.Ok);
   end Set_Listener;

   --  Decodes a 64 byte set-listener reply as a Set_Listener_Addr_Port_Response record.
   function Set_Listener_Addr_Port (Reply : Packet) return Responses.Set_Listener_Addr_Port_Response is
      --!format off
      R : Replies.Set_Listener_Addr_Port_Response with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Listener'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => R.Ok);
   end Set_Listener_Addr_Port;

   --  Decodes a 64 byte get-status reply as a Get_Status_Response record.
   function Get_Status (Reply : Packet) return Responses.Get_Status_Response is
      --!format off
      R : Replies.Get_Status_Response with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Status'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return
        (Controller           => R.Controller,
         System_Date          => Unpack_Short_Date (R.System_Date),
         System_Time          => Unpack_Time (R.System_Time),
         Door_1_Open          => Unpack_Boolean (R.Door_1_Open),
         Door_2_Open          => Unpack_Boolean (R.Door_2_Open),
         Door_3_Open          => Unpack_Boolean (R.Door_3_Open),
         Door_4_Open          => Unpack_Boolean (R.Door_4_Open),
         Door_1_Button        => Unpack_Boolean (R.Door_1_Button),
         Door_2_Button        => Unpack_Boolean (R.Door_2_Button),
         Door_3_Button        => Unpack_Boolean (R.Door_3_Button),
         Door_4_Button        => Unpack_Boolean (R.Door_4_Button),
         Relays               => Relay_State (R.Relays),
         Inputs               => Inputs_State (R.Inputs),
         System_Error         => R.System_Error,
         Special_Info         => R.Special_Info,
         Event_Index          => R.Event_Index,
         Event_Type           => R.Event_Type,
         Event_Access_Granted => Unpack_Boolean (R.Event_Access_Granted),
         Event_Door           => R.Event_Door,
         Event_Direction      => R.Event_Direction,
         Event_Card           => R.Event_Card,
         Event_Timestamp      => Unpack_Date_Time (R.Event_Timestamp),
         Event_Reason         => R.Event_Reason,
         Sequence_No          => R.Sequence_No);
   end Get_Status;

   --  Decodes a 64 byte get-door reply as a Get_Door_Response record.
   function Get_Door (Reply : Packet) return Responses.Get_Door_Response is
      --!format off
      R : Replies.Get_Door_Response with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Door'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Door => R.Door, Mode => R.Mode, Open_Delay => R.Open_Delay);
   end Get_Door;

   --  Decodes a 64 byte set-door reply as a Set_Door_Response record.
   function Set_Door (Reply : Packet) return Responses.Set_Door_Response is
      --!format off
      R : Replies.Set_Door_Response with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Door'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Door => R.Door, Mode => R.Mode, Open_Delay => R.Open_Delay);
   end Set_Door;

   --  Translates a BCD coded version to a vN.NN formatted string.
   function Unpack_Version (V : Version_Field) return Unbounded_String is
      N1 : constant Integer := Integer (Shift_Right (V.Major, 4) and 16#0F#);
      N2 : constant Integer := Integer (Shift_Right (V.Major, 0) and 16#0F#);
      N3 : constant Integer := Integer (Shift_Right (V.Minor, 4) and 16#0F#);
      N4 : constant Integer := Integer (Shift_Right (V.Minor, 0) and 16#0F#);

      Major : constant Integer := N1 * 10 + N2;
      Minor : constant Integer := N3 * 10 + N4;
   begin
      return To_Unbounded_String ("v" & Trim (Major'Image, Left) & "." & Trim (Minor'Image, Left));
   end Unpack_Version;

   --  Decodes a 64 byte set-door-passcodes reply as a Get_Door_Passcodes_Response record.
   function Set_Door_Passcodes (Reply : Packet) return Responses.Set_Door_Passcodes_Response is
      --!format off
      R : Replies.Set_Door_Passcodes_Reply with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Door_Passcodes'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Set_Door_Passcodes;

   --  Decodes a 64 byte open-door reply as an Open_Door_Response record.
   function Open_Door (Reply : Packet) return Responses.Open_Door_Response is
      --!format off
      R : Replies.Open_Door_Reply with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Open_Door'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Open_Door;

   --  Decodes a 64 byte get-cards reply as an Get_Cards_Response record.
   function Get_Cards (Reply : Packet) return Responses.Get_Cards_Response is
      --!format off
      R : Replies.Get_Cards_Reply with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Cards'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Cards => R.Cards);
   end Get_Cards;

   --  Decodes a 64 byte get-card reply as an Get_Card_Response record.
   function Get_Card (Reply : Packet) return Responses.Get_Card_Response is
      --!format off
      R : Replies.Get_Card_Reply with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Card'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return
        (Controller => R.Controller,
         Card       => R.Card,
         Start_Date => Unpack_Date (R.Start_Date),
         End_Date   => Unpack_Date (R.End_Date),
         Door_1     => R.Door_1,
         Door_2     => R.Door_2,
         Door_3     => R.Door_3,
         Door_4     => R.Door_4,
         PIN        => R.PIN);
   end Get_Card;

   --  Decodes a 64 byte get-card-at-index reply as an Get_Card_Response record.
   function Get_Card_At_Index (Reply : Packet) return Responses.Get_Card_At_Index_Response is
      --!format off
      R : Replies.Get_Card_At_Index_Reply with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Card_At_Index'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return
        (Controller => R.Controller,
         Card       => R.Card,
         Start_Date => Unpack_Date (R.Start_Date),
         End_Date   => Unpack_Date (R.End_Date),
         Door_1     => R.Door_1,
         Door_2     => R.Door_2,
         Door_3     => R.Door_3,
         Door_4     => R.Door_4,
         PIN        => R.PIN);
   end Get_Card_At_Index;

   --  Decodes a 64 byte put-card reply as a Put_Card_Response record.
   function Put_Card (Reply : Packet) return Responses.Put_Card_Response is
      --!format off
      R : Replies.Put_Card_Reply with Import, Address => Reply'Address;
      --!format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Put_Card'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Put_Card;

   --  Decodes a 64 byte put-card reply as a Delete_Card_Response record.
   function Delete_Card (Reply : Packet) return Responses.Delete_Card_Response is
      R : Replies.Delete_Card_Reply with Import, Address => Reply'Address;
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Delete_Card'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Delete_Card;

   --  Decodes a 64 byte put-card reply as a Delete_All_Cards_Response record.
   function Delete_All_Cards (Reply : Packet) return Responses.Delete_All_Cards_Response is
      --  !format off
      R : Replies.Delete_All_Cards_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Delete_All_Cards'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Delete_All_Cards;

   --  Decodes a 64 byte get-event reply as a Get_Status_Response record.
   function Get_Event (Reply : Packet) return Responses.Get_Event_Response is
      --  !format off
      R : Replies.Get_Event_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Event'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return
        (Controller     => R.Controller,
         Index          => R.Index,
         Event_Type     => R.Event_Type,
         Access_Granted => Unpack_Boolean (R.Access_Granted),
         Door           => R.Door,
         Direction      => R.Direction,
         Card           => R.Card,
         Timestamp      => Unpack_Date_Time (R.Timestamp),
         Reason         => R.Reason);
   end Get_Event;

   --  Decodes a 64 byte get-event-index reply as a Get_Event_Index_Response record.
   function Get_Event_Index (Reply : Packet) return Responses.Get_Event_Index_Response is
      --  !format off
      R : Replies.Get_Event_Index_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Event_Index'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Index => R.Index);
   end Get_Event_Index;

   --  Decodes a 64 byte set-event-index reply as a Set_Event_Index_Response record.
   function Set_Event_Index (Reply : Packet) return Responses.Set_Event_Index_Response is
      --  !format off
      R : Replies.Set_Event_Index_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Event_Index'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Set_Event_Index;

   --  Decodes a 64 byte record-special-events reply as a Record_Special_Events_Response record.
   function Record_Special_Events (Reply : Packet) return Responses.Record_Special_Events_Response is
      --  !format off
      R : Replies.Record_Special_Events_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Record_Special_Events'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Record_Special_Events;

   --  Decodes a 64 byte rget-time-profile reply as a Get_Time_Profile_Response record.
   function Get_Time_Profile (Reply : Packet) return Responses.Get_Time_Profile_Response is
      --  !format off
      R : Replies.Get_Time_Profile_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Time_Profile'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return
        (Controller      => R.Controller,
         Profile         => R.Profile,
         Start_Date      => Unpack_Date (R.Start_Date),
         End_Date        => Unpack_Date (R.End_Date),
         Monday          => Unpack_Boolean (R.Monday),
         Tuesday         => Unpack_Boolean (R.Tuesday),
         Wednesday       => Unpack_Boolean (R.Wednesday),
         Thursday        => Unpack_Boolean (R.Thursday),
         Friday          => Unpack_Boolean (R.Friday),
         Saturday        => Unpack_Boolean (R.Saturday),
         Sunday          => Unpack_Boolean (R.Sunday),
         Segment_1_Start => Unpack_HHmm (R.Segment_1_Start),
         Segment_1_End   => Unpack_HHmm (R.Segment_1_End),
         Segment_2_Start => Unpack_HHmm (R.Segment_2_Start),
         Segment_2_End   => Unpack_HHmm (R.Segment_2_End),
         Segment_3_Start => Unpack_HHmm (R.Segment_3_Start),
         Segment_3_End   => Unpack_HHmm (R.Segment_3_End),
         Linked_Profile  => R.Linked_Profile);
   end Get_Time_Profile;

   --  Decodes a 64 byte set-time-profile reply as a Set_Time_Profile_Response record.
   function Set_Time_Profile (Reply : Packet) return Responses.Set_Time_Profile_Response is
      --  !format off
      R : Replies.Set_Time_Profile_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Time_Profile'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Set_Time_Profile;

   --  Decodes a 64 byte clear-time-profiles reply as a Clear_Time_Profiles_Response record.
   function Clear_Time_Profiles (Reply : Packet) return Responses.Clear_Time_Profiles_Response is
      --  !format off
      R : Replies.Clear_Time_Profiles_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Clear_Time_Profiles'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Clear_Time_Profiles;

   --  Decodes a 64 byte add-task reply as a Add_Task_Response record.
   function Add_Task (Reply : Packet) return Responses.Add_Task_Response is
      --  !format off
      R : Replies.Add_Task_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Add_Task'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Add_Task;

   --  Decodes a 64 byte refresh-tasklist reply as a Refresh_Task_List_Response record.
   function Refresh_Task_List (Reply : Packet) return Responses.Refresh_Task_List_Response is
      --  !format off
      R : Replies.Refresh_Task_List_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Refresh_Task_List'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Refresh_Task_List;

   --  Decodes a 64 byte clear-tasklist reply as a Clear_Task_List_Response record.
   function Clear_Task_List (Reply : Packet) return Responses.Clear_Task_List_Response is
      --  !format off
      R : Replies.Clear_Task_List_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Clear_Task_List'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Clear_Task_List;

   --  Decodes a 64 byte set-pc-control reply as a Set_PC_Control_Response record.
   function Set_PC_Control (Reply : Packet) return Responses.Set_PC_Control_Response is
      --  !format off
      R : Replies.Set_PC_Control_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_PC_Control'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Set_PC_Control;

   --  Decodes a 64 byte set-interlock reply as a Set_Interlock_Response record.
   function Set_Interlock (Reply : Packet) return Responses.Set_Interlock_Response is
      --  !format off
      R : Replies.Set_Interlock_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Interlock'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Set_Interlock;

   --  Decodes a 64 byte activate-keypads reply as a Activate_Keypads_Response record.
   function Activate_Keypads (Reply : Packet) return Responses.Activate_Keypads_Response is
      --  !format off
      R : Replies.Activate_Keypads_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Activate_Keypads'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Activate_Keypads;

   --  Decodes a 64 byte get-antipassback reply as a Get_Antipassback_Response record.
   function Get_Antipassback (Reply : Packet) return Responses.Get_Antipassback_Response is
      --  !format off
      R : Replies.Get_Antipassback_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Get_Antipassback'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Antipassback => R.Antipassback);
   end Get_Antipassback;

   --  Decodes a 64 byte set-antipassback reply as a Set_Antipassback_Response record.
   function Set_Antipassback (Reply : Packet) return Responses.Set_Antipassback_Response is
      --  !format off
      R : Replies.Set_Antipassback_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_Antipassback'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Set_Antipassback;

   --  Decodes a 64 byte set-firstcard reply as a Set_First_Card_Response record.
   function Set_First_Card (Reply : Packet) return Responses.Set_First_Card_Response is
      --  !format off
      R : Replies.Set_First_Card_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Set_First_Card'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Set_First_Card;

   --  Decodes a 64 byte restore-default-parameters reply as a Restore_Default_Parameters_Response record.
   function Restore_Default_Parameters (Reply : Packet) return Responses.Restore_Default_Parameters_Response is
      --  !format off
      R : Replies.Restore_Default_Parameters_Reply with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= Codec.Restore_Default_Parameters'Enum_Rep then
         raise Invalid_Response_Error;
      end if;

      return (Controller => R.Controller, Ok => Unpack_Boolean (R.Ok));
   end Restore_Default_Parameters;

   --  Decodes a 64 byte listener-event message as a Listener_Event record.
   function Listener_Event (Reply : Packet) return Responses.Listener_Event is
      --  !format off
      R : Replies.Listener_Event with Import, Address => Reply'Address;
      --  !format on
   begin
      if R.SOM /= Codec.SOM and then (R.SOM /= Codec.SOM_v6_62 or else R.Opcode /= 16#20#) then
         raise Invalid_Response_Error;
      end if;

      if R.Opcode /= 16#20# then
         raise Invalid_Response_Error;
      end if;

      return
        (Controller           => R.Controller,
         System_Date          => Unpack_Short_Date (R.System_Date),
         System_Time          => Unpack_Time (R.System_Time),
         Door_1_Open          => Unpack_Boolean (R.Door_1_Open),
         Door_2_Open          => Unpack_Boolean (R.Door_2_Open),
         Door_3_Open          => Unpack_Boolean (R.Door_3_Open),
         Door_4_Open          => Unpack_Boolean (R.Door_4_Open),
         Door_1_Button        => Unpack_Boolean (R.Door_1_Button),
         Door_2_Button        => Unpack_Boolean (R.Door_2_Button),
         Door_3_Button        => Unpack_Boolean (R.Door_3_Button),
         Door_4_Button        => Unpack_Boolean (R.Door_4_Button),
         Relays               => Relay_State (R.Relays),
         Inputs               => Inputs_State (R.Inputs),
         System_Error         => R.System_Error,
         Special_Info         => R.Special_Info,
         Event_Index          => R.Event_Index,
         Event_Type           => R.Event_Type,
         Event_Access_Granted => Unpack_Boolean (R.Event_Access_Granted),
         Event_Door           => R.Event_Door,
         Event_Direction      => R.Event_Direction,
         Event_Card           => R.Event_Card,
         Event_Timestamp      => Unpack_Date_Time (R.Event_Timestamp),
         Event_Reason         => R.Event_Reason,
         Sequence_No          => R.Sequence_No);
   end Listener_Event;

   --  Translates an Unsigned_8 into a Boolean - 1 is True, anything else is False.
   function Unpack_Boolean (B : Unsigned_8) return Boolean is
   begin
      if B = 1 then
         return True;
      else
         return False;
      end if;
   end Unpack_Boolean;

   --  Translates a BCD coded date to a DateOnly.
   function Unpack_Date (Bytes : BCD) return DateOnly is
      YYYYMMDD : constant String := BCD_To_String (Bytes);
      YYYY     : constant Unsigned_16 := Unsigned_16'Value (YYYYMMDD (1 .. 4));
      MM       : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD (5 .. 6));
      DD       : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD (7 .. 8));
   begin
      if YYYY = 0 and then MM = 0 and then DD = 0 then
         return (Year => 1, Month => 1, Day => 1);
      else
         return (Year => YYYY, Month => MM, Day => DD);
      end if;
   end Unpack_Date;

   --  Translates a BCD coded YYMMDD date to a DateOnly.
   function Unpack_Short_Date (Bytes : BCD) return DateOnly is
      YYMMDD : constant String := BCD_To_String (Bytes);
      YY     : constant Unsigned_8 := Unsigned_8'Value (YYMMDD (1 .. 2));
      MM     : constant Unsigned_8 := Unsigned_8'Value (YYMMDD (3 .. 4));
      DD     : constant Unsigned_8 := Unsigned_8'Value (YYMMDD (5 .. 6));
   begin
      return (Year => 2_000 + Unsigned_16 (YY), Month => MM, Day => DD);
   end Unpack_Short_Date;

   --  Translates a BCD coded time to a TimeOnly.
   function Unpack_Time (Bytes : BCD) return TimeOnly is
      HHMMSS : constant String := BCD_To_String (Bytes);
      HH     : constant Unsigned_8 := Unsigned_8'Value (HHMMSS (1 .. 2));
      MM     : constant Unsigned_8 := Unsigned_8'Value (HHMMSS (3 .. 4));
      SS     : constant Unsigned_8 := Unsigned_8'Value (HHMMSS (5 .. 6));
   begin
      return (Hour => HH, Minute => MM, Second => SS);
   end Unpack_Time;

   --  Translates a BCD coded date/time to a DateTime.
   function Unpack_Date_Time (Bytes : BCD) return DateTime is
      YYYYMMDD_HHMMSS : constant String := BCD_To_String (Bytes);

      Year   : constant Unsigned_16 := Unsigned_16'Value (YYYYMMDD_HHMMSS (1 .. 4));
      Month  : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD_HHMMSS (5 .. 6));
      Day    : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD_HHMMSS (7 .. 8));
      Hour   : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD_HHMMSS (9 .. 10));
      Minute : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD_HHMMSS (11 .. 12));
      Second : constant Unsigned_8 := Unsigned_8'Value (YYYYMMDD_HHMMSS (13 .. 14));
   begin
      if Year = 0 and then Month = 0 and then Day = 0 and then Hour = 0 and then Minute = 0 and then Second = 0 then
         return (Year => 1, Month => 1, Day => 1, Hour => 0, Minute => 0, Second => 0);
      end if;

      return (Year => Year, Month => Month, Day => Day, Hour => Hour, Minute => Minute, Second => Second);
   end Unpack_Date_Time;

   --  Translates a BCD coded time to an HHmm.
   function Unpack_HHmm (Bytes : BCD) return HHmm is
      HHMM : constant String := BCD_To_String (Bytes);
      HH   : constant Unsigned_8 := Unsigned_8'Value (HHMM (1 .. 2));
      MM   : constant Unsigned_8 := Unsigned_8'Value (HHMM (3 .. 4));
   begin
      return (Hour => HH, Minute => MM);
   end Unpack_HHmm;

   --  Translates a BCD coded string in a byte array to a string.
   function BCD_To_String (Bytes : BCD) return String is
      Hex : constant String := "0123456789";
      S   : String (1 .. Bytes'Length * 2);
      I   : Positive := 1;
   begin
      for B of Bytes loop
         declare
            MSB : constant Integer := Integer (Shift_Right (B, 4));
            LSB : constant Integer := Integer (B and 16#0F#);
         begin
            S (I) := Hex (MSB + 1);
            S (I + 1) := Hex (LSB + 1);
            I := I + 2;
         end;
      end loop;

      return S;
   end BCD_To_String;

end Uhppoted.Lib.Decode;
