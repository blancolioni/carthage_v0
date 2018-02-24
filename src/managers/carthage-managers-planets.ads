with Carthage.Houses;
with Carthage.Planets;
with Carthage.Stacks;

package Carthage.Managers.Planets is

   function Create_Active_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type;
      Meta   : Stack_Meta_Manager_Access)
      return Manager_Type;

   function Create_Passive_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type;
      Meta   : Stack_Meta_Manager_Access)
      return Manager_Type;

end Carthage.Managers.Planets;
