private with Memor.Database;

limited with Carthage.Planets;

with Carthage.Colours;
with Carthage.Objects.Localised;

package Carthage.Houses is

   type House_Category is (Noble, Church, League, Imperial,
                           Vau, Symbiot, Rebels);

   type House_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Category
     (House : House_Record)
      return House_Category;

   function Capital
     (House : House_Record)
      return access constant Carthage.Planets.Planet_Record'Class;

   function Colour
     (House : House_Record)
      return Carthage.Colours.Colour_Type;

   subtype House_Class is House_Record'Class;

   type House_Type is access constant House_Record'Class;

   function Number_Of_Houses
     return Natural;

   procedure Scan
     (Process : not null access procedure (House : House_Type));

private

   type House_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with
      record
         Category : House_Category;
         Capital  : access constant Carthage.Planets.Planet_Record'Class;
         Colour   : Carthage.Colours.Colour_Type;
      end record;

   overriding function Object_Database
     (Item : House_Record)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       (Class_Name        => "house",
        Element_Type      => House_Record,
        Element_Reference => House_Type);

   overriding function Object_Database
     (Item : House_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Number_Of_Houses
     return Natural
   is (Db.Active_Count);

   function Colour
     (House : House_Record)
      return Carthage.Colours.Colour_Type
   is (House.Colour);

   function Category
     (House : House_Record)
      return House_Category
   is (House.Category);

   function Capital
     (House : House_Record)
      return access constant Carthage.Planets.Planet_Record'Class
   is (House.Capital);

end Carthage.Houses;
