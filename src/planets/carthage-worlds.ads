private with Memor.Database;
private with Memor.Element_Vectors;

with Carthage.Climate;
with Carthage.Objects;
with Carthage.Resources;
with Carthage.Terrain;
with Carthage.Units;

package Carthage.Worlds is

   subtype Frequency_Range is Float range 0.0 .. 1.0;

   type World_Record is
     new Carthage.Objects.Root_Identifier_Object with private;

   function Smoothness (World : World_Record) return Natural;

   function Average_Temperature
     (World : World_Record)
      return Carthage.Climate.Temperature_Range;

   procedure Scan_Terrain_Frequency
     (World : World_Record;
      Process : not null access
        procedure (Terrain : Carthage.Terrain.Terrain_Type;
                   Frequency : Frequency_Range));

   function Base_Land_Terrain
     (World       : World_Record;
      Temperature : Carthage.Climate.Temperature_Range;
      Humidity    : Carthage.Climate.Humidity_Range)
      return Carthage.Terrain.Terrain_Type;

   subtype Multiplier is Float range 0.0 .. 5.0;

   function Movement_Multiplier
     (World   : World_Record;
      Terrain : Carthage.Terrain.Terrain_Type;
      Unit    : Carthage.Units.Unit_Category)
      return Multiplier;

   function Road_Movement_Multiplier
     (World : World_Record;
      Unit  : Carthage.Units.Unit_Category)
      return Multiplier;

   subtype World_Class is World_Record'Class;

   type World_Type is access constant World_Record'Class;

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return World_Type
   with Pre => Exists (Id);

private

   type Terrain_Frequency is
      record
         Terrain   : Carthage.Terrain.Terrain_Type;
         Frequency : Frequency_Range;
      end record;

   type Frequency_Array is array (Positive range <>) of Terrain_Frequency;

   type Climate_Terrain is
      record
         Terrain       : Carthage.Terrain.Terrain_Type;
         Humidity_Low  : Carthage.Climate.Humidity_Range;
         Humidity_High : Carthage.Climate.Humidity_Range;
         Temp_Low      : Carthage.Climate.Temperature_Range;
         Temp_High     : Carthage.Climate.Temperature_Range;
      end record;

   type Climate_Terrain_Array is array (Positive range <>) of Climate_Terrain;

   type Resource_Info is
      record
         Resource : Carthage.Resources.Resource_Type;
         Factor   : Float;
      end record;

   type Unit_Movement_Array is
     array (Carthage.Units.Unit_Category) of Multiplier;

   type Terrain_Info is
      record
         Movement : Unit_Movement_Array := (others => 1.0);
      end record;

   package Terrain_Vectors is
     new Memor.Element_Vectors (Carthage.Terrain.Terrain_Record, Terrain_Info,
                                (others => <>));

   type World_Record is
     new Carthage.Objects.Root_Identifier_Object with
      record
         Index           : Positive;
         Smoothness      : Positive;
         Ave_Temperature : Carthage.Climate.Temperature_Range;
         Base_Land       : Carthage.Terrain.Terrain_Type;
         Terrain         : access Frequency_Array;
         Climate_Terrain : access Climate_Terrain_Array;
         Terrain_Info    : Terrain_Vectors.Vector;
         Road_Movement   : Unit_Movement_Array;
      end record;

   overriding function Object_Database
     (Item : World_Record)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       (Class_Name        => "world",
        Element_Type      => World_Record,
        Element_Reference => World_Type);

   overriding function Object_Database
     (Item : World_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return World_Type
   is (Db.Get (Id));

   function Smoothness (World : World_Record) return Natural
   is (World.Smoothness);

   function Average_Temperature
     (World : World_Record)
      return Carthage.Climate.Temperature_Range
   is (World.Ave_Temperature);

   function Movement_Multiplier
     (World   : World_Record;
      Terrain : Carthage.Terrain.Terrain_Type;
      Unit    : Carthage.Units.Unit_Category)
      return Multiplier
   is (World.Terrain_Info.Element (Terrain).Movement (Unit));

   function Road_Movement_Multiplier
     (World : World_Record;
      Unit  : Carthage.Units.Unit_Category)
      return Multiplier
   is (World.Road_Movement (Unit));

end Carthage.Worlds;
