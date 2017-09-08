private with Memor.Database;
private with Memor.Element_Vectors;

with Carthage.Objects.Localised;

package Carthage.Resources is

   type Resource_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Base_Price (Resource : Resource_Record) return Positive;

   subtype Resource_Class is Resource_Record'Class;

   type Resource_Type is access constant Resource_Record'Class;

   function Food return Resource_Type;

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return Resource_Type
   with Pre => Exists (Id);

   procedure Scan
     (Process : not null access procedure (Resource : Resource_Type));

   type Stock_Interface is limited interface;

   function Quantity (Stock    : Stock_Interface;
                      Resource : not null access constant Resource_Class)
                      return Natural
                      is abstract;

   procedure Set_Quantity
     (Stock        : in out Stock_Interface;
      Resource     : not null access constant Resource_Class;
      New_Quantity : Natural)
   is abstract;

   procedure Clear_Stock
     (Stock : in out Stock_Interface'Class);

   procedure Scan_Stock
     (Stock : Stock_Interface'Class;
      Process : not null access
        procedure (Resource : Resource_Type;
                   Quantity : Natural));

   procedure Add
     (Stock          : in out Stock_Interface'Class;
      Resource       : not null access constant Resource_Class;
      Added_Quantity : Natural);

   procedure Remove
     (Stock            : in out Stock_Interface'Class;
      Resource         : not null access constant Resource_Class;
      Removed_Quantity : Natural)
     with Pre => Removed_Quantity <= Stock.Quantity (Resource);

   type Stock_Record is new Stock_Interface with private;

   overriding function Quantity
     (Stock    : Stock_Record;
      Resource : not null access constant Resource_Class)
      return Natural;

   overriding procedure Set_Quantity
     (Stock        : in out Stock_Record;
      Resource     : not null access constant Resource_Class;
      New_Quantity : Natural);

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

   function Base_Price (Resource : Resource_Record) return Positive
   is (Resource.Price);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return Resource_Type
   is (Db.Get (Id));

   package Stock_Vectors is
     new Memor.Element_Vectors (Resource_Record, Natural, 0);

   type Stock_Record is new Stock_Interface with
      record
         Vector : Stock_Vectors.Vector;
      end record;

   overriding function Quantity
     (Stock    : Stock_Record;
      Resource : not null access constant Resource_Class)
      return Natural
   is (Stock.Vector.Element (Resource));

   function Food return Resource_Type
   is (Get ("food"));

end Carthage.Resources;
