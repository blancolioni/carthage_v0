package Carthage.Tiles.Configure is

   function Create_Tile
     (Index           : Positive;
      Position        : Tile_Position;
      Height          : Integer;
      Base_Terrain    : Carthage.Terrain.Terrain_Type;
      Feature_Terrain : Carthage.Terrain.Terrain_Type)
      return Tile_Type;

   type Terrain_Array is
     array (Positive range <>) of Carthage.Terrain.Terrain_Type;

   function Create_Tile
     (Index    : Positive;
      Position : Tile_Position;
      Terrain  : Terrain_Array;
      Road     : Boolean;
      River    : Boolean)
      return Tile_Type;

end Carthage.Tiles.Configure;
