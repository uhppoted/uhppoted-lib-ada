with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Registry is
   type Handler is access procedure;

   type Command_Set is tagged private;

   function Initialise return Command_Set;
   procedure Execute (Self : Command_Set; Cmd : String);

private
   package Command_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Handler,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Command_Set is tagged record
      Internal_Map : Command_Maps.Map;
   end record;   

end Registry;
