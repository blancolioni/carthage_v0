package body Carthage.Stacks.Create is

   Current_Stack_Id : String := "A0-A0A0";

   function Next_Stack_Id return String;

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
         Stack.Create_With_Identity (Next_Stack_Id);
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

   -------------------
   -- Next_Stack_Id --
   -------------------

   function Next_Stack_Id return String is
   begin
      for Ch of reverse Current_Stack_Id loop
         if Ch = '9' then
            Ch := '0';
         elsif Ch = 'Z' then
            Ch := 'A';
         elsif Ch in '0' .. '8'
           or else Ch in 'A' .. 'Z'
         then
            Ch := Character'Succ (Ch);
            exit;
         end if;
      end loop;
      return Current_Stack_Id;
   end Next_Stack_Id;

end Carthage.Stacks.Create;
