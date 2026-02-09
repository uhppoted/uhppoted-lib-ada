with Ada.Assertions;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;

with Uhppoted.Lib.Integration_Tests;

procedure Integration_Tests is
   use Ada.Text_IO;
   use Uhppoted.Lib.Integration_Tests;
begin
   Test_Find_Controllers;

exception
   when E : Ada.Assertions.Assertion_Error =>
      Put_Line ("");
      Put_Line ("   *** FAILED  " & Ada.Exceptions.Exception_Message (E));
      Put_Line ("");
      Ada.Command_Line.Set_Exit_Status (1);

   when E : others =>
      Put_Line ("");
      Put_Line ("   *** ERROR " & Ada.Exceptions.Exception_Name (E));
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Put_Line ("");
      Ada.Command_Line.Set_Exit_Status (1);

end Integration_Tests;
