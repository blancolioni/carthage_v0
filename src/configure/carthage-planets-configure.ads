with Tropos;

package Carthage.Planets.Configure is

   procedure Create_Surface_Graph;

   procedure Configure_Planet
     (Config : Tropos.Configuration);

   procedure Configure_Position
     (Planet_Id : String;
      X, Y      : Coordinate);

end Carthage.Planets.Configure;
