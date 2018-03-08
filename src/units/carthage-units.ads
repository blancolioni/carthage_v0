private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

private with Memor.Database;
private with Memor.Element_Vectors;

with Carthage.Objects.Localised;

with Carthage.Resources;
with Carthage.Technology;

package Carthage.Units is

   type Unit_Category is
     (Jump, Space, Lander, Naval, Hover, Air, Foot, Tread, Wheel, Crawler);

   subtype Space_Category is Unit_Category range Jump .. Lander;
   subtype Ground_Category is Unit_Category range Naval .. Crawler;

   type Weapon_Category is
     (Water, Indirect, Air, Direct, Close, Psy,
      Ranged_Space, Direct_Space, Close_Space);

   function Can_Target
     (Weapon : Weapon_Category;
      Unit   : Unit_Category)
      return Boolean;

   type Unit_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Index
     (Unit : Unit_Record)
      return Natural;

   function Category
     (Unit : Unit_Record)
      return Unit_Category;

   function Space_Unit
     (Unit : Unit_Record'Class)
      return Boolean
   is (Unit.Category in Space_Category);

   function Ground_Unit
     (Unit : Unit_Record'Class)
      return Boolean
   is (Unit.Category in Ground_Category);

   function Is_Sceptre (Unit : Unit_Record'Class) return Boolean;
   function Is_Noble (Unit : Unit_Record'Class) return Boolean;

   function Movement
     (Unit : Unit_Record)
      return Natural;

   function Spot
     (Unit : Unit_Record)
      return Natural;

   function Agility
     (Unit : Unit_Record)
      return Natural;

   function Armour
     (Unit : Unit_Record)
      return Natural;

   function Psychic_Defense
     (Unit : Unit_Record)
      return Natural;

   function Eat
     (Unit : Unit_Record)
      return Resource_Quantity;

   function Image_Resource
     (Unit : Unit_Record)
      return String;

   function Has_Attack
     (Unit   : Unit_Record;
      Weapon : Weapon_Category)
      return Boolean;

   function Accuracy
     (Unit   : Unit_Record;
      Weapon : Weapon_Category)
      return Positive
     with Pre => Unit.Has_Attack (Weapon);

   function Strength
     (Unit   : Unit_Record;
      Weapon : Weapon_Category)
      return Positive
     with Pre => Unit.Has_Attack (Weapon);

   function Rank
     (Unit : Unit_Record)
      return Natural;

   subtype Unit_Class is Unit_Record'Class;

   type Unit_Type is access constant Unit_Record'Class;

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return Unit_Type
     with Pre => Exists (Id);

   function Get (Index : Natural) return Unit_Type;

private

   type Weapon_Record is
      record
         Accuracy : Natural := 0;
         Strength : Natural := 0;
      end record;

   type Weapon_Array is array (Weapon_Category) of Weapon_Record;

   package Resource_Cost_Vectors is
     new Memor.Element_Vectors
       (Index_Type    => Carthage.Resources.Resource_Record,
        Element_Type  => Natural,
        Default_Value => 0);

   type Enabling_Tech is array (1 .. 4) of Carthage.Technology.Technology_Type;

   type Unit_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with
      record
         Index          : Natural := 0;
         Category       : Unit_Category;
         Is_Sceptre     : Boolean := False;
         Is_Noble       : Boolean := False;
         Move           : Natural := 0;
         Spot           : Natural := 0;
         Camouflage     : Natural := 0;
         Agility        : Natural := 0;
         Armour         : Natural := 0;
         Psy_Defence    : Natural := 0;
         Weapons        : Weapon_Array := (others => (0, 0));
         Cargo          : Natural := 0;
         Can_Be_Cargo   : Boolean := False;
         Combat         : Boolean := False;
         Maintenance    : Natural := 0;
         Credit_Cost    : Natural := 0;
         Eat            : Resource_Quantity := 0.0;
         Rank           : Natural := 0;
         Resource_Cost  : Resource_Cost_Vectors.Vector;
         Build_Unit     : Unit_Type := null;
         Enabled_By     : Enabling_Tech := (others => null);
         Turns_To_Build : Natural := 0;
         Image_Resource : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Object_Database
     (Item : Unit_Record)
      return Memor.Memor_Database;

   function Index
     (Unit : Unit_Record)
      return Natural
   is (Unit.Index);

   function Category
     (Unit : Unit_Record)
      return Unit_Category
   is (Unit.Category);

   function Movement
     (Unit : Unit_Record)
      return Natural
   is (Unit.Move);

   function Spot
     (Unit : Unit_Record)
      return Natural
   is (Unit.Spot);

   function Agility
     (Unit : Unit_Record)
      return Natural
   is (Unit.Agility);

   function Armour
     (Unit : Unit_Record)
      return Natural
   is (Unit.Armour);

   function Psychic_Defense
     (Unit : Unit_Record)
      return Natural
   is (Unit.Psy_Defence);

   function Eat
     (Unit : Unit_Record)
      return Resource_Quantity
   is (Unit.Eat);

   function Rank
     (Unit : Unit_Record)
      return Natural
   is (Unit.Rank);

   function Has_Attack
     (Unit   : Unit_Record;
      Weapon : Weapon_Category)
      return Boolean
   is (Unit.Weapons (Weapon).Accuracy > 0);

   function Accuracy
     (Unit   : Unit_Record;
      Weapon : Weapon_Category)
      return Positive
   is (Unit.Weapons (Weapon).Accuracy);

   function Strength
     (Unit   : Unit_Record;
      Weapon : Weapon_Category)
      return Positive
   is (Unit.Weapons (Weapon).Strength);

   function Image_Resource
     (Unit : Unit_Record)
      return String
   is (Ada.Strings.Unbounded.To_String (Unit.Image_Resource));

   package Db is
     new Memor.Database
       (Class_Name        => "unit",
        Element_Type      => Unit_Record,
        Element_Reference => Unit_Type);

   overriding function Object_Database
     (Item : Unit_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return Unit_Type
   is (Db.Get (Id));

   function Is_Sceptre (Unit : Unit_Record'Class) return Boolean
   is (Unit.Is_Sceptre);

   function Is_Noble (Unit : Unit_Record'Class) return Boolean
   is (Unit.Is_Noble);

   package Unit_Vectors is
     new Ada.Containers.Vectors (Natural, Unit_Type);

   Us : Unit_Vectors.Vector;

   function Get (Index : Natural) return Unit_Type
   is (Us.Element (Index));

   Weapon_Targets : array (Unit_Category, Weapon_Category) of Boolean;

   function Can_Target
     (Weapon : Weapon_Category;
      Unit   : Unit_Category)
      return Boolean
   is (Weapon_Targets (Unit, Weapon));

end Carthage.Units;
