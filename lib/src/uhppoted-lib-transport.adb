with Ada.Text_IO;

package body Uhppoted.Lib.Transport is
   use Ada.Text_IO;

   --  Converts a byte to hex.
   function Hex (B : Unsigned_8) return String;

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

   --  Prints out a hex dump of a 64 byte packet.
   procedure Dump (Msg : String; P : Uhppoted.Lib.Types.Packet) is
   begin
      Put_Line (Msg);
      for I in P'Range loop
         begin
            case I is
               when 1                 => Put ("00000000  " & Hex (P (I)));
               when 17                => Put ("00000010  " & Hex (P (I)));
               when 33                => Put ("00000020  " & Hex (P (I)));
               when 49                => Put ("00000030  " & Hex (P (I)));
               when 9  | 25 | 41 | 57 => Put ("  " & Hex (P (I)));
               when 16 | 32 | 48 | 64 => Put_Line (" " & Hex (P (I)));
               when others            => Put (" " & Hex (P (I)));
            end case;
         end;
      end loop;
      Put_Line ("");
   end Dump;

   --  Converts a byte to hex.
   function Hex (B : Unsigned_8) return String is
      Charset : constant array (Interfaces.Unsigned_8 range 0 .. 15) of Character :=
        ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];

      LSN : constant Unsigned_8 := B mod 16;
      MSN : constant Unsigned_8 := B / 16;
   begin
      return Charset (MSN) & Charset (LSN);
   end Hex;

end Uhppoted.Lib.Transport;
