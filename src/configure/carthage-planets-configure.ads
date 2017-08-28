with Tropos;

package Carthage.Planets.Configure is

   procedure Create_Surface_Graph;

   procedure Configure_Planet
     (Config : Tropos.Configuration);

   procedure Configure_Position
     (Planet_Id : String;
      X, Y      : Coordinate);

   procedure Import_Planet
     (Name     : String;
      X, Y     : Natural;
      Tile_Set : Natural;
      Create_Tile : not null access
        function (X : Tile_X;
                  Y : Tile_Y)
      return Carthage.Tiles.Tile_Type);

   procedure Import_Jump_Gate
     (X1, Y1, X2, Y2 : Natural);

end Carthage.Planets.Configure;
