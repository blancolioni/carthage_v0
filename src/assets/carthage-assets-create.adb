package body Carthage.Assets.Create is

   function New_Asset
     (Unit      : Carthage.Units.Unit_Type;
      Owner     : Carthage.Houses.House_Type;
      XP        : Asset_Experience := Green;
      Loyalty   : Asset_Loyalty := Asset_Loyalty'Last;
      Health    : Asset_Health := Asset_Health'Last)
      return Asset_Type
   is
      procedure Create (Asset : in out Asset_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Asset : in out Asset_Class) is
      begin
         Asset.Create_With_Identity
           (Owner.Identifier & "-" & Unit.Identifier);
         Asset.Set_Name (Unit.Identifier);
         Asset.Owner := Owner;
         Asset.Unit := Unit;
         Asset.Experience := XP;
         Asset.Loyalty := Loyalty;
         Asset.Health := Health;
      end Create;

      Asset : constant Asset_Type :=
               Db.Create (Create'Access);
   begin
      return Asset;
   end New_Asset;

end Carthage.Assets.Create;
