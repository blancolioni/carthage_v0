package Carthage.Assets.Create is

   function New_Asset
     (Unit      : Carthage.Units.Unit_Type;
      Owner     : Carthage.Houses.House_Type;
      XP        : Asset_Experience := Green;
      Loyalty   : Loyalty_Type := Loyalty_Type'Last;
      Health    : Health_Type := Health_Type'Last)
      return Asset_Type;

end Carthage.Assets.Create;
