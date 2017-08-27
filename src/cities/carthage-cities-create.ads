package Carthage.Cities.Create is

   function New_City
     (Planet   : Carthage.Planets.Planet_Type;
      Tile     : Carthage.Tiles.Tile_Type;
      Structure : Carthage.Structures.Structure_Type;
      Owner    : Carthage.Houses.House_Type)
      return City_Type
     with Pre => not Tile.Has_City,
       Post => Tile.Has_City and then Tile.City = New_City'Result;

   procedure New_City
     (Planet   : Carthage.Planets.Planet_Type;
      Tile     : Carthage.Tiles.Tile_Type;
      Structure : Carthage.Structures.Structure_Type;
      Owner    : Carthage.Houses.House_Type)
     with Pre => not Tile.Has_City,
       Post => Tile.Has_City;

end Carthage.Cities.Create;
