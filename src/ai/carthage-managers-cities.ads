with Carthage.Planets;

package Carthage.Managers.Cities is

   function City_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Manager_Type;

end Carthage.Managers.Cities;
