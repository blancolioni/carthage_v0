with Carthage.Houses;
with Carthage.Planets;

private package Carthage.UI.Models.Planets is

   function Planet_Model
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Lui.Models.Object_Model;

end Carthage.UI.Models.Planets;
