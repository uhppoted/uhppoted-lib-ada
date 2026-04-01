with ArgParse;

package Handlers is
   procedure Find_Controllers (Args : ArgParse.Args);
   procedure Get_Controller (Args : ArgParse.Args);
   procedure Set_IPv4 (Args : ArgParse.Args);
   procedure Get_Time (Args : ArgParse.Args);
   procedure Set_Time (Args : ArgParse.Args);
   procedure Get_Listener (Args : ArgParse.Args);
   procedure Set_Listener (Args : ArgParse.Args);
   procedure Get_Status (Args : ArgParse.Args);
   procedure Get_Door (Args : ArgParse.Args);
   procedure Set_Door (Args : ArgParse.Args);
   procedure Set_Door_Passcodes (Args : ArgParse.Args);
   procedure Open_Door (Args : ArgParse.Args);
   procedure Get_Cards (Args : ArgParse.Args);
end Handlers;
