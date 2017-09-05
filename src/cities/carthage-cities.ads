private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Memor.Database;

with Carthage.Structures;
with Carthage.Houses;
with Carthage.Planets;
with Carthage.Resources;
with Carthage.Tiles;

with Carthage.Objects;

package Carthage.Cities is

   type City_Record is
     new Carthage.Objects.Root_Named_Object
     and Carthage.Resources.Stock_Interface
   with private;

   overriding function Quantity
     (City     : City_Record;
      Resource : not null access constant
        Carthage.Resources.Resource_Class)
      return Natural;

   overriding procedure Set_Quantity
     (City         : in out City_Record;
      Resource     : not null access constant
        Carthage.Resources.Resource_Class;
      New_Quantity : Natural);

   overriding function Log_Identifier
     (City : City_Record)
      return String;

   function Planet
     (City : City_Record)
      return Carthage.Planets.Planet_Type;

   function Tile
     (City : City_Record)
      return Carthage.Tiles.Tile_Type;

   function Structure
     (City : City_Record)
      return Carthage.Structures.Structure_Type;

   function Owner
     (City : City_Record)
      return Carthage.Houses.House_Type;

   function Is_Agora
     (City : City_Record)
      return Boolean;

   function Seen_By
     (City  : City_Record;
      House : Carthage.Houses.House_Type)
      return Boolean;

   procedure Buy_Resource
     (City     : in out City_Record;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Positive);

   procedure Sell_Resource
     (City     : in out City_Record;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : Positive);

   subtype City_Class is City_Record'Class;

   type City_Type is access constant City_Record'Class;

   procedure Scan_Planet_Cities
     (Planet  : Carthage.Planets.Planet_Type;
      Process : not null access
        procedure (City : City_Type));

   procedure Update_City
     (City : City_Type;
      Update : not null access
        procedure (City : in out City_Class));

   procedure Set_Seen_By
     (City  : City_Type;
      House : Carthage.Houses.House_Type);

   procedure Scan_Cities
     (Process : not null access procedure (City : City_Type));

   procedure Scan_Cities
     (Structure : Carthage.Structures.Structure_Type;
      Process   : not null access procedure (City : City_Type));

   procedure Scan_Cities
     (Owner   : Carthage.Houses.House_Type;
      Process : not null access procedure (City : City_Type));

   procedure Scan_Cities
     (Test    : not null access function (City : City_Type) return Boolean;
      Process : not null access procedure (City : City_Type));

   type Updateable_Reference (City : not null access City_Record'Class)
   is private with Implicit_Dereference => City;

   function Update
     (Item : not null access constant City_Record'Class)
      return Updateable_Reference;

private

   type City_Order_Class is (Buy, Sell);

   type City_Order_Record (Class : City_Order_Class) is
      record
         case Class is
            when Buy | Sell =>
               Resource : Carthage.Resources.Resource_Type;
               Quantity : Positive;
         end case;
      end record;

   package City_Order_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (City_Order_Record);

   type City_Record is
     new Carthage.Objects.Root_Named_Object
     and Carthage.Resources.Stock_Interface with
      record
         Owner     : Carthage.Houses.House_Type;
         Planet    : Carthage.Planets.Planet_Type;
         Tile      : Carthage.Tiles.Tile_Type;
         Structure : Carthage.Structures.Structure_Type;
         Seen      : Carthage.Houses.House_Set;
         Stock     : Carthage.Resources.Stock_Record;
         Orders    : City_Order_Lists.List;
      end record;

   overriding function Object_Database
     (Item : City_Record)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       (Class_Name        => "city",
        Element_Type      => City_Record,
        Element_Reference => City_Type);

   overriding function Object_Database
     (Item : City_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   overriding function Quantity
     (City     : City_Record;
      Resource : not null access constant
        Carthage.Resources.Resource_Class)
      return Natural
   is (City.Stock.Quantity (Resource));

   function Planet
     (City : City_Record)
      return Carthage.Planets.Planet_Type
   is (City.Planet);

   function Tile
     (City : City_Record)
      return Carthage.Tiles.Tile_Type
   is (City.Tile);

   function Structure
     (City : City_Record)
      return Carthage.Structures.Structure_Type
   is (City.Structure);

   function Owner
     (City : City_Record)
      return Carthage.Houses.House_Type
   is (City.Owner);

   function Seen_By
     (City  : City_Record;
      House : Carthage.Houses.House_Type)
      return Boolean
   is (Carthage.Houses.Element (City.Seen, House));

   function Is_Agora
     (City : City_Record)
      return Boolean
   is (City.Identifier = "agora");

   type Updateable_Reference (City : not null access City_Record'Class) is
      record
         Update : Db.Updateable_Reference (City);
      end record;

end Carthage.Cities;
