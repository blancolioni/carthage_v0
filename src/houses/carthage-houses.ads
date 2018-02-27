private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Memor.Database;

limited with Carthage.Planets;

with Carthage.Colors;
with Carthage.Objects.Localised;

package Carthage.Houses is

   type House_Category is (Noble, Church, League, Imperial,
                           Vau, Symbiot, Rebels);

   type Treaty_Status is (Allied, Neutral, Hostile, War);

   type House_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Category
     (House : House_Record)
      return House_Category;

   function Treaty_Status_With
     (House : House_Record;
      Other : not null access constant House_Record'Class)
      return Treaty_Status;

   function At_War_With
     (House : House_Record;
      Other : not null access constant House_Record'Class)
      return Boolean;

   function Capital
     (House : House_Record)
      return access constant Carthage.Planets.Planet_Record'Class;

   function Color
     (House : House_Record)
      return Carthage.Colors.Color_Type;

   procedure Scan_Known_Planets
     (House : House_Record;
      Process : not null access
        procedure (Planet_Id : String));

   procedure Spend
     (House  : in out House_Record;
      Amount : Positive);

   procedure Earn
     (House  : in out House_Record;
      Amount : Positive);

   procedure Log_Status
     (House : House_Record'Class);

   type House_Manager_Interface is interface;

   procedure Set_House_Manager
     (House   : in out House_Record;
      Manager : not null access House_Manager_Interface'Class);

   subtype House_Class is House_Record'Class;

   type House_Type is access constant House_Record'Class;

   function Number_Of_Houses
     return Natural;

   procedure Scan
     (Process : not null access procedure
        (House : House_Record'Class));

   procedure Scan
     (Process : not null access procedure
        (House : House_Type));

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return House_Type
     with Pre => Exists (Id);

   type House_Set is private;

   procedure Clear (Set : in out House_Set);
   procedure Insert (Set   : in out House_Set;
                     House : House_Type);
   procedure Remove (Set   : in out House_Set;
                     House : House_Type);
   function Element (Set   : House_Set;
                     House : House_Type)
                     return Boolean;

   type Updateable_Reference (Item : not null access House_Record'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant House_Record'Class)
      return Updateable_Reference;

private

   Max_Houses : constant := 32;

   type House_Set is mod 2 ** Max_Houses;

   package Planet_Id_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type House_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with
      record
         Category      : House_Category;
         Capital       : access constant Carthage.Planets.Planet_Record'Class;
         Color        : Carthage.Colors.Color_Type;
         Set_Flag      : House_Set;
         Known_Planets : Planet_Id_Lists.List;
         Manager       : access House_Manager_Interface'Class;
         Cash          : Natural := 5_000;
         Debt          : Natural := 0;
         Tax_Rate      : Float   := 0.1;
         Tithe_Skim    : Float   := 0.1;
         Unit_Pay      : Float   := 0.75;
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

   function Color
     (House : House_Record)
      return Carthage.Colors.Color_Type
   is (House.Color);

   function Category
     (House : House_Record)
      return House_Category
   is (House.Category);

   function Capital
     (House : House_Record)
      return access constant Carthage.Planets.Planet_Record'Class
   is (House.Capital);

   function Treaty_Status_With
     (House : House_Record;
      Other : not null access constant House_Record'Class)
      return Treaty_Status
   is (if House.Category = Rebels xor Other.Category = Rebels
       then War
       else Neutral);

   function At_War_With
     (House : House_Record;
      Other : not null access constant House_Record'Class)
      return Boolean
   is (House.Treaty_Status_With (Other) = War);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return House_Type
   is (Db.Get (Id));

   function Element (Set   : House_Set;
                     House : House_Type)
                     return Boolean
   is ((Set and House.Set_Flag) /= 0);

   type Updateable_Reference (Item : not null access House_Record'Class) is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Carthage.Houses;
