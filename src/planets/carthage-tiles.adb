with Carthage.Cities;

package body Carthage.Tiles is

   -----------------
   -- Description --
   -----------------

   function Description
     (Tile : Tile_Record)
      return String
   is
      use type Carthage.Terrain.Terrain_Type;
      Terrain : constant String :=
        (if Tile.Terrain (2) /= null
         and then Tile.Terrain (2) /= Tile.Terrain (1)
         then Tile.Terrain (1).Identifier
         & " " & Tile.Terrain (2).Identifier
         else Tile.Terrain (1).Identifier);
      City : constant String :=
               (if Tile.Has_City
                then Tile.City.Structure.Identifier else "");
   begin
      return City & " " & Terrain;
   end Description;

   --------------
   -- Set_City --
   --------------

   procedure Set_City
     (Tile : Tile_Type;
      City : not null access constant Carthage.Cities.City_Record'Class)
   is
      procedure Update (Rec : in out Tile_Class);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Tile_Class) is
      begin
         Rec.City := City;
      end Update;

   begin
      Db.Update (Tile.Reference, Update'Access);
   end Set_City;

   --------------
   -- Set_Road --
   --------------

   procedure Set_Road
     (Tile : Tile_Type;
      Road : Boolean)
   is
      procedure Update (Rec : in out Tile_Class);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Tile_Class) is
      begin
         Rec.Road := Road;
      end Update;

   begin
      Db.Update (Tile.Reference, Update'Access);
   end Set_Road;

   ---------------
   -- Set_Stack --
   ---------------

   procedure Set_Stack
     (Tile  : Tile_Type;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
      procedure Update (Rec : in out Tile_Class);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Tile_Class) is
      begin
         Rec.Stack := Stack;
      end Update;

   begin
      Db.Update (Tile.Reference, Update'Access);
   end Set_Stack;

end Carthage.Tiles;
