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
         Tile.Terrain := (Base_Terrain, Feature_Terrain, null);
         Tile.Road := False;
         Tile.River := False;
         Carthage.Houses.Clear (Tile.Seen);
      end Create;

   begin
      return Db.Create (Create'Access);
   end Create_Tile;

   -----------------
   -- Create_Tile --
   -----------------

   function Create_Tile
     (Index    : Positive;
      Position : Tile_Position;
      Terrain  : Terrain_Array;
      Road     : Boolean;
      River    : Boolean)
      return Tile_Type
   is
      procedure Create (Tile : in out Tile_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Tile : in out Tile_Class) is
         Ts : constant Terrain_Array (1 .. Terrain'Length) := Terrain;
      begin
         Tile.Index := Index;
         Tile.Position := Position;
         Tile.Height := 0;
         Tile.Road := Road;
         Tile.River := River;
         Tile.Terrain := (others => null);
         for I in Ts'Range loop
            Tile.Terrain (Terrain_Layer (I)) := Ts (I);
         end loop;
         Carthage.Houses.Clear (Tile.Seen);

      end Create;

   begin
      return Db.Create (Create'Access);
   end Create_Tile;

end Carthage.Tiles.Configure;
