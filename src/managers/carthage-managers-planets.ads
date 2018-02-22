with Carthage.Houses;
with Carthage.Planets;

package Carthage.Managers.Planets is

   function Create_Active_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Manager_Type;

   function Create_Passive_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Manager_Type;

end Carthage.Managers.Planets;
