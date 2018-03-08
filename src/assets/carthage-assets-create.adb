with Carthage.Identifiers;

package body Carthage.Assets.Create is

   Asset_Id_Template : constant String := "A0-AA-A0A0";

   ---------------
   -- New_Asset --
   ---------------

   function New_Asset
     (Unit      : Carthage.Units.Unit_Type;
      Owner     : Carthage.Houses.House_Type;
      XP        : Asset_Experience := Green;
      Loyalty   : Loyalty_Type := Loyalty_Type'Last;
      Health    : Health_Type := Health_Type'Last)
      return Asset_Type
   is
      procedure Create (Asset : in out Asset_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Asset : in out Asset_Class) is
      begin
         Asset.Create_With_Identity
           (Carthage.Identifiers.New_Identifier (Asset_Id_Template)
            & "-" & Owner.Identifier & "-" & Unit.Identifier);
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
