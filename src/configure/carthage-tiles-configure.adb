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
         Ts : Terrain_Array (1 .. Terrain'Length) := Terrain;
      begin
         Tile.Index := Index;
         Tile.Position := Position;
         Tile.Height := 0;
         Tile.Road := Road;
         Tile.River := River;

         if not Ts (1).Base then
            for I in 2 .. Ts'Last loop
               if Ts (I).Base then
                  declare
                     It : constant Carthage.Terrain.Terrain_Type :=
                            Ts (1);
                  begin
                     Ts (1) := Ts (I);
                     Ts (I) := It;
                  end;
                  exit;
               end if;
            end loop;
         end if;

         Tile.Terrain := (others => null);
         for I in Ts'Range loop
            Tile.Terrain (Terrain_Layer (I)) := Ts (I);
         end loop;

      end Create;

   begin
      return Db.Create (Create'Access);
   end Create_Tile;

end Carthage.Tiles.Configure;
