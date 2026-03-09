package body Uhppoted.Lib.Transport is
   --  Creates the wrapped selector handle.
   overriding procedure Initialize (E : in out H) is
   begin
      Create_Selector (E.Selector);
   end Initialize;

   --  Closes the wrapped selector handle.
   overriding procedure Finalize (E : in out H) is
   begin
      Close_Selector (E.Selector);
   end Finalize;

end Uhppoted.Lib.Transport;
