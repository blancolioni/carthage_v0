package Carthage.Stacks.Create is

   function New_Ground_Stack
     (Owner     : Carthage.Houses.House_Type;
      Planet    : Carthage.Planets.Planet_Type;
      Tile      : Carthage.Tiles.Tile_Type)
      return Stack_Type
     with Pre => not Tile.Has_Stack,
     Post => Tile.Has_Stack and then Tile.Stack = New_Ground_Stack'Result;

   function New_Orbital_Stack
     (Owner     : Carthage.Houses.House_Type;
      Planet    : Carthage.Planets.Planet_Type)
      return Stack_Type;

end Carthage.Stacks.Create;
