package Carthage.Stacks.Create is

   function New_Ground_Stack
     (Owner     : Carthage.Houses.House_Type;
      Planet    : Carthage.Planets.Planet_Type;
      Tile      : Carthage.Tiles.Tile_Type)
      return Stack_Type
     with Post => Tile.Has_Stacks
       and then Carthage.Tiles."=" (New_Ground_Stack'Result.Tile, Tile);

   function New_Orbital_Stack
     (Owner     : Carthage.Houses.House_Type;
      Planet    : Carthage.Planets.Planet_Type)
      return Stack_Type;

end Carthage.Stacks.Create;
