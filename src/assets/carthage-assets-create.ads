package Carthage.Assets.Create is

   function New_Asset
     (Unit      : Carthage.Units.Unit_Type;
      Owner     : Carthage.Houses.House_Type;
      XP        : Asset_Experience := Green;
      Loyalty   : Asset_Loyalty := Asset_Loyalty'Last;
      Health    : Asset_Health := Asset_Health'Last)
      return Asset_Type;

end Carthage.Assets.Create;
