package body Carthage.Stacks.Create is

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
