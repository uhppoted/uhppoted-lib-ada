package Uhppoted.Lib is

   type Controller_ID is new Positive;
   type Controller_Array is array (Positive range <>) of Controller_ID;

   -- Stub function to retrieve a list of controller IDs
   function Get_Controllers return Controller_Array;

end Uhppoted.Lib;
