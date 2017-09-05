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

   type Weapon_Category is
     (Water, Indirect, Air, Direct, Close, Psy,
      Ranged_Space, Direct_Space, Close_Space);

   type Unit_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Index
     (Unit : Unit_Record)
      return Positive;

   function Category
     (Unit : Unit_Record)
      return Unit_Category;

   function Movement
     (Unit : Unit_Record)
      return Natural;

   function Spot
     (Unit : Unit_Record)
      return Natural;

   function Eat
     (Unit : Unit_Record)
      return Natural;

   function Image_Resource
     (Unit : Unit_Record)
      return String;

   subtype Unit_Class is Unit_Record'Class;

   type Unit_Type is access constant Unit_Record'Class;

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return Unit_Type
     with Pre => Exists (Id);

   function Get (Index : Natural) return Unit_Type;

private

   type Weapon_Record is
      record
         Accuracy : Natural;
         Strength : Natural;
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
         Index          : Natural;
         Category       : Unit_Category;
         Move           : Natural;
         Spot           : Natural;
         Camouflage     : Natural;
         Agility        : Natural;
         Armour         : Natural;
         Psy_Defence    : Natural;
         Weapons        : Weapon_Array;
         Cargo          : Natural;
         Can_Be_Cargo   : Boolean;
         Combat         : Boolean;
         Maintenance    : Natural;
         Credit_Cost    : Natural;
         Eat            : Natural;
         Resource_Cost  : Resource_Cost_Vectors.Vector;
         Build_Unit     : Unit_Type;
         Enabled_By     : Enabling_Tech;
         Turns_To_Build : Natural;
         Image_Resource : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Object_Database
     (Item : Unit_Record)
      return Memor.Memor_Database;

   function Index
     (Unit : Unit_Record)
      return Positive
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

   function Eat
     (Unit : Unit_Record)
      return Natural
   is (Unit.Eat);

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

   package Unit_Vectors is
     new Ada.Containers.Vectors (Natural, Unit_Type);

   Us : Unit_Vectors.Vector;

   function Get (Index : Natural) return Unit_Type
   is (Us.Element (Index));

end Carthage.Units;
