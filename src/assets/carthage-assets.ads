private with Memor.Database;

with Carthage.Houses;
with Carthage.Resources;
with Carthage.Units;

with Carthage.Objects;

package Carthage.Assets is

   type Asset_Experience is
     (Green, Expert, Elite);

   type Asset_Health is range 0 .. 100;
   type Asset_Loyalty is range 0 .. 100;

   type Asset_Record is
     new Carthage.Objects.Root_Named_Object
     and Carthage.Resources.Stock_Interface
   with private;

   overriding function Quantity
     (Asset    : Asset_Record;
      Resource : not null access constant
        Carthage.Resources.Resource_Class)
      return Resource_Quantity;

   overriding procedure Set_Quantity
     (Asset        : in out Asset_Record;
      Resource     : not null access constant
        Carthage.Resources.Resource_Class;
      New_Quantity : Resource_Quantity);

   function Owner
     (Asset : Asset_Record)
      return Carthage.Houses.House_Type;

   function Unit
     (Asset : Asset_Record)
      return Carthage.Units.Unit_Type;

   function Movement
     (Asset : Asset_Record)
      return Natural;

   function Spot
     (Asset : Asset_Record)
      return Natural;

   function Health
     (Asset : Asset_Record)
      return Asset_Health;

   function Alive
     (Asset : Asset_Record)
      return Boolean;

   procedure Damage
     (Asset  : in out Asset_Record;
      Points : Positive);

   subtype Asset_Class is Asset_Record'Class;

   type Asset_Type is access constant Asset_Record'Class;

   type Updateable_Reference (Item : not null access Asset_Record'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Asset_Record'Class)
      return Updateable_Reference;

private

   type Asset_Record is
     new Carthage.Objects.Root_Named_Object
     and Carthage.Resources.Stock_Interface with
      record
         Owner       : Carthage.Houses.House_Type;
         Unit        : Carthage.Units.Unit_Type;
         Health      : Asset_Health := Asset_Health'Last;
         Loyalty     : Asset_Loyalty := Asset_Loyalty'Last;
         Experience  : Asset_Experience := Asset_Experience'First;
         Movement    : Natural := 0;
         Stock       : Carthage.Resources.Stock_Record;
      end record;

   overriding function Object_Database
     (Item : Asset_Record)
      return Memor.Memor_Database;

   overriding function Quantity
     (Asset    : Asset_Record;
      Resource : not null access constant
        Carthage.Resources.Resource_Class)
      return Resource_Quantity
   is (Asset.Stock.Quantity (Resource));

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

   function Spot
     (Asset : Asset_Record)
      return Natural
   is (Asset.Unit.Spot);

   function Health
     (Asset : Asset_Record)
      return Asset_Health
   is (Asset.Health);

   function Alive
     (Asset : Asset_Record)
      return Boolean
   is (Asset.Health > 0);

   type Updateable_Reference (Item : not null access Asset_Record'Class) is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Carthage.Assets;
