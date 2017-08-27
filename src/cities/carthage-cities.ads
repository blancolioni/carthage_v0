private with Memor.Database;

with Carthage.Structures;
with Carthage.Houses;
with Carthage.Planets;
with Carthage.Tiles;

with Carthage.Objects;

package Carthage.Cities is

   type City_Record is
     new Carthage.Objects.Root_Named_Object with private;

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

   subtype City_Class is City_Record'Class;

   type City_Type is access constant City_Record'Class;

   procedure Scan_Planet_Cities
     (Planet  : Carthage.Planets.Planet_Type;
      Process : not null access
        procedure (City : City_Type));

private

   type City_Record is
     new Carthage.Objects.Root_Named_Object with
      record
         Owner    : Carthage.Houses.House_Type;
         Planet   : Carthage.Planets.Planet_Type;
         Tile     : Carthage.Tiles.Tile_Type;
         Structure : Carthage.Structures.Structure_Type;
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

end Carthage.Cities;
