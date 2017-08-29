with Carthage.Planets;

package Carthage.Managers.Assets is

   function Ground_Asset_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Manager_Type;

end Carthage.Managers.Assets;
