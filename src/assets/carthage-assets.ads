private with Memor.Database;

with Carthage.Houses;
with Carthage.Resources;
with Carthage.Units;

with Carthage.Objects;

package Carthage.Assets is

   type Asset_Experience is
     (Green, Expert, Elite);

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

   function Space_Asset
     (Asset : Asset_Record'Class)
      return Boolean
   is (Asset.Unit.Space_Unit);

   function Ground_Asset
     (Asset : Asset_Record'Class)
      return Boolean
   is (Asset.Unit.Ground_Unit);

   function Movement
     (Asset : Asset_Record)
      return Natural;

   function Spot
     (Asset : Asset_Record)
      return Natural;

   function Health
     (Asset : Asset_Record)
      return Health_Type;

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

   type Asset_Container_Interface is limited interface;

   function Is_Full
     (Container : Asset_Container_Interface)
      return Boolean
      is abstract;

   function Is_Empty
     (Container : Asset_Container_Interface)
      return Boolean
      is abstract;

   function First_Asset
     (Container : Asset_Container_Interface)
      return Asset_Type
   is abstract
     with Pre'Class => not Container.Is_Empty;

   function Owner
     (Container : Asset_Container_Interface)
      return Carthage.Houses.House_Type
   is abstract
     with Post'Class => Carthage.Houses."/=" (Owner'Result, null);

   function Has_Asset
     (Container : Asset_Container_Interface;
      Asset     : Carthage.Assets.Asset_Type)
      return Boolean
      is abstract
     with Post'Class => not Has_Asset'Result or else not Container.Is_Empty;

   procedure Add_Asset
     (Container : in out Asset_Container_Interface;
      Asset     : Carthage.Assets.Asset_Type)
     is abstract
     with Pre'Class => not Container.Is_Full
     and then Carthage.Houses."=" (Container.Owner, Asset.Owner)
     and then not Container.Has_Asset (Asset),
     Post'class => Container.Has_Asset (Asset);

   procedure Remove_Asset
     (Container : in out Asset_Container_Interface;
      Asset     : Carthage.Assets.Asset_Type)
     is abstract
     with Pre'class => Container.Has_Asset (Asset),
     Post'Class => not Container.Has_Asset (Asset);

   function Variable_Reference
     (Container : not null access constant Asset_Container_Interface)
      return access Asset_Container_Interface'Class
      is abstract;

   function Container
     (Asset : Asset_Record'Class)
      return access constant Asset_Container_Interface'Class;

   procedure Move_To
     (Asset     : not null access constant Asset_Record'Class;
      Container : not null access constant Asset_Container_Interface'Class);

private

   type Asset_Record is
     new Carthage.Objects.Root_Named_Object
     and Carthage.Resources.Stock_Interface with
      record
         Container   : access constant Asset_Container_Interface'Class;
         Owner       : Carthage.Houses.House_Type;
         Unit        : Carthage.Units.Unit_Type;
         Health      : Health_Type := Health_Type'Last;
         Loyalty     : Loyalty_Type := Loyalty_Type'Last;
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
      return Health_Type
   is (Asset.Health);

   function Alive
     (Asset : Asset_Record)
      return Boolean
   is (Asset.Health > 0);

   function Container
     (Asset : Asset_Record'Class)
      return access constant Asset_Container_Interface'Class
   is (Asset.Container);

   type Updateable_Reference (Item : not null access Asset_Record'Class) is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Carthage.Assets;
