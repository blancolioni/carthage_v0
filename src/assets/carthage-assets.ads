private with Memor.Database;

with Carthage.Houses;
with Carthage.Units;

with Carthage.Objects;

package Carthage.Assets is

   type Asset_Experience is
     (Green, Expert, Elite);

   type Asset_Health is range 0 .. 100;
   type Asset_Loyalty is range 0 .. 100;

   type Asset_Record is
     new Carthage.Objects.Root_Named_Object with private;

   function Owner
     (Asset : Asset_Record)
      return Carthage.Houses.House_Type;

   function Unit
     (Asset : Asset_Record)
      return Carthage.Units.Unit_Type;

   function Movement
     (Asset : Asset_Record)
      return Natural;

   subtype Asset_Class is Asset_Record'Class;

   type Asset_Type is access constant Asset_Record'Class;

private

   type Asset_Record is
     new Carthage.Objects.Root_Named_Object with
      record
         Owner       : Carthage.Houses.House_Type;
         Unit        : Carthage.Units.Unit_Type;
         Health      : Asset_Health;
         Loyalty     : Asset_Loyalty;
         Experience  : Asset_Experience;
         Movement    : Natural;
      end record;

   overriding function Object_Database
     (Item : Asset_Record)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       (Class_Name        => "asset",
        Element_Type      => Asset_Record,
        Element_Reference => Asset_Type);

   overriding function Object_Database
     (Item : Asset_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Unit
     (Asset : Asset_Record)
      return Carthage.Units.Unit_Type
   is (Asset.Unit);

   function Owner
     (Asset : Asset_Record)
      return Carthage.Houses.House_Type
   is (Asset.Owner);

   function Movement
     (Asset : Asset_Record)
      return Natural
   is (Asset.Unit.Movement);

end Carthage.Assets;
