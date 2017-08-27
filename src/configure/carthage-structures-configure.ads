with Tropos;

package Carthage.Structures.Configure is

   procedure Configure_Structure
     (Config : Tropos.Configuration);

   procedure Configure_Bonus_Production
     (Config : Tropos.Configuration);

   function Random_Bonus
     (Planet_Category : String;
      Terrain_Id      : String)
     return Structure_Type;

end Carthage.Structures.Configure;
