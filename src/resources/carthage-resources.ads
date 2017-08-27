private with Memor.Database;

with Carthage.Objects.Localised;

package Carthage.Resources is

   type Resource_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   subtype Resource_Class is Resource_Record'Class;

   type Resource_Type is access constant Resource_Record'Class;

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return Resource_Type
   with Pre => Exists (Id);

private

   type Resource_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with
      record
         Price : Natural;
      end record;

   overriding function Object_Database
     (Item : Resource_Record)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       (Class_Name        => "resource",
        Element_Type      => Resource_Record,
        Element_Reference => Resource_Type);

   overriding function Object_Database
     (Item : Resource_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return Resource_Type
   is (Db.Get (Id));

end Carthage.Resources;
