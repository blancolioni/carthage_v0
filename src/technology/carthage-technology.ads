private with Ada.Containers.Vectors;

private with Memor.Database;

with Carthage.Objects.Localised;

package Carthage.Technology is

   type Technology_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Index
     (Technology : Technology_Record)
      return Positive;

   subtype Technology_Class is Technology_Record'Class;

   type Technology_Type is access constant Technology_Record'Class;

   function Get (Index : Natural) return Technology_Type;

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return Technology_Type
     with Pre => Exists (Id);

private

   type Enabling_Tech is array (1 .. 3) of Technology_Type;

   type Technology_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with
      record
         Top_Level  : Boolean;
         Index      : Natural;
         Enabled_By : Enabling_Tech;
         Cost       : Natural;
         Like       : Natural;
      end record;

   overriding function Object_Database
     (Item : Technology_Record)
      return Memor.Memor_Database;

   function Index
     (Technology : Technology_Record)
      return Positive
   is (Technology.Index);

   package Db is
     new Memor.Database
       (Class_Name        => "technology",
        Element_Type      => Technology_Record,
        Element_Reference => Technology_Type);

   overriding function Object_Database
     (Item : Technology_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return Technology_Type
   is (Db.Get (Id));

   package Technology_Vectors is
     new Ada.Containers.Vectors (Natural, Technology_Type);

   Tech : Technology_Vectors.Vector;

   function Get (Index : Natural) return Technology_Type
   is (Tech.Element (Index));

end Carthage.Technology;
