with Ada.Strings.Unbounded;
with Uhppoted.Lib.Types;
with Uhppoted.Lib.Responses;
with Uhppoted.Lib.Replies;

package Uhppoted.Lib.Decode is

   function Get_Controller (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Controller_Response;
   --  Decodes a 64 byte get-controller reply as a Get_Controller_Response record.

   function Set_IPv4 (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_IPv4_Response;
   --  Decodes a 64 byte set-IPv4 reply as a Set_IPv4_Response record.

   function Get_Time (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Time_Response;
   --  Decodes a 64 byte get-time reply as a Get_Time_Response record.

   function Set_Time (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Time_Response;
   --  Decodes a 64 byte set-time reply as a Set_Time_Response record.

   function Get_Listener (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Listener_Response;
   --  Decodes a 64 byte get-listener reply as a Get_Listener_Response record.

   function Get_Listener_Addr_Port (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Listener_Addr_Port_Response;
   --  Decodes a 64 byte get-listener reply as a Get_Listener_Addr_Port_Response record.

   function Set_Listener (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Listener_Response;
   --  Decodes a 64 byte set-listener reply as a Set_Listener_Response record.

   function Set_Listener_Addr_Port (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Listener_Addr_Port_Response;
   --  Decodes a 64 byte set-listener reply as a Set_Listener_Response record.

   function Get_Status (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Status_Response;
   --  Decodes a 64 byte get-status reply as a Get_Status_Response record.

   function Get_Door (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Door_Response;
   --  Decodes a 64 byte get-door reply as a Get_Door_Response record.

   function Set_Door (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Door_Response;
   --  Decodes a 64 byte set-door reply as a Get_Door_Response record.

   function Set_Door_Passcodes (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Set_Door_Passcodes_Response;
   --  Decodes a 64 byte set-door-passcodes reply as a Get_Door_Passcodes_Response record.

   function Open_Door (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Open_Door_Response;
   --  Decodes a 64 byte open-door reply as an Open_Door_Response record.

   function Get_Cards (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Cards_Response;
   --  Decodes a 64 byte get-cards reply as a Get_Cards_Response record.

   function Get_Card (Reply : Uhppoted.Lib.Types.Packet)
      return Uhppoted.Lib.Responses.Get_Card_Response;
   --  Decodes a 64 byte get-card reply as a Get_Card_Response record.

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

end Uhppoted.Lib.Decode;
