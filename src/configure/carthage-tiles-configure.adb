package body Carthage.Tiles.Configure is

   -----------------
   -- Create_Tile --
   -----------------

   function Create_Tile
     (Index           : Positive;
      Position        : Tile_Position;
      Height          : Integer;
      Base_Terrain    : Carthage.Terrain.Terrain_Type;
      Feature_Terrain : Carthage.Terrain.Terrain_Type)
      return Tile_Type
   is
      procedure Create (Tile : in out Tile_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Tile : in out Tile_Class) is
      begin
         Tile.Index := Index;
         Tile.Position := Position;
         Tile.Height := Height;
         Tile.Terrain := (Base_Terrain, Feature_Terrain);
         Tile.Road := False;
      end Create;

   begin
      return Db.Create (Create'Access);
   end Create_Tile;

   function Create_Tile
     (Index    : Positive;
      Position : Tile_Position;
      Terrain  : Terrain_Array)
      return Tile_Type
   is
      procedure Create (Tile : in out Tile_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Tile : in out Tile_Class) is
      begin
         Tile.Index := Index;
         Tile.Position := Position;
         Tile.Height := 0;
         Tile.Terrain (1) := Terrain (1);
         if Terrain'Last > 1 then
            Tile.Terrain (2) := Terrain (2);
         end if;
         Tile.Road := False;
      end Create;

   begin
      return Db.Create (Create'Access);
   end Create_Tile;

end Carthage.Tiles.Configure;
