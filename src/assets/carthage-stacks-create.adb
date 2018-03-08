with Carthage.Identifiers;

package body Carthage.Stacks.Create is

   Stack_Id_Template : constant String := "A0-A0A0";

   ----------------------
   -- New_Ground_Stack --
   ----------------------

   function New_Ground_Stack
     (Owner     : Carthage.Houses.House_Type;
      Planet    : Carthage.Planets.Planet_Type;
      Tile      : Carthage.Tiles.Tile_Type)
      return Stack_Type
   is
      procedure Create (Stack : in out Stack_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Stack : in out Stack_Class) is
      begin
         Stack.Create_With_Identity
           (Carthage.Identifiers.New_Identifier (Stack_Id_Template)
            & "-" & Owner.Identifier);
         Stack.Set_Name (Owner.Name);
         Stack.Owner := Owner;
         Stack.Planet := Planet;
         Stack.Tile := Tile;
         Stack.Count := 0;
      end Create;

      Stack : constant Stack_Type :=
                Db.Create (Create'Access);
   begin
      Tile.Update.Add_Stack (Stack);
      return Stack;
   end New_Ground_Stack;

   -----------------------
   -- New_Orbital_Stack --
   -----------------------

   function New_Orbital_Stack
     (Owner     : Carthage.Houses.House_Type;
      Planet    : Carthage.Planets.Planet_Type)
      return Stack_Type
   is
      procedure Create (Stack : in out Stack_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Stack : in out Stack_Class) is
      begin
         Stack.Create_With_Identity
           ("orbit-" & Planet.Identifier & "-" & Owner.Identifier);
         Stack.Set_Name (Owner.Name);
         Stack.Owner := Owner;
         Stack.Planet := Planet;
         Stack.Tile := null;
         Stack.Count := 0;
      end Create;

      Stack : constant Stack_Type :=
                Db.Create (Create'Access);
   begin
      return Stack;
   end New_Orbital_Stack;

end Carthage.Stacks.Create;
