private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

private with WL.String_Maps;

private with Memor.Database;

with Carthage.Planets;
with Carthage.Resources;
with Carthage.Technology;
with Carthage.Terrain;
with Carthage.Tiles;
with Carthage.Worlds;

with Carthage.Objects.Localised;

package Carthage.Structures is

   Chance_Against : constant := 1024;

   type Structure_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Terrain_OK
     (Structure : Structure_Record;
      Terrain  : Carthage.Terrain.Terrain_Type)
      return Boolean;

   function Chance
     (Structure       : Structure_Record;
      Planet_Category : String;
      Terrain_Id      : String)
      return Natural;

   function Radius
     (Structure : Structure_Record)
      return Natural;

   function Is_Bonus
     (Structure : Structure_Record)
      return Boolean;

   function Is_Harvester
     (Structure : Structure_Record)
      return Boolean;

   function Is_Agora
     (Structure : Structure_Record)
      return Boolean;

   function Is_Palace
     (Structure : Structure_Record)
      return Boolean;

   function Is_Shield
     (Structure : Structure_Record)
      return Boolean;

   type Resource_Quantity_Record is
      record
         Resource : Carthage.Resources.Resource_Type;
         Quantity : Resource_Quantity;
      end record;

   type Production_Array is
     array (Positive range <>) of Resource_Quantity_Record;

   function Production_Inputs
     (Structure : Structure_Record)
      return Production_Array;

   function Production_Outputs
     (Structure : Structure_Record)
      return Production_Array;

   function Produces
     (Structure : Structure_Record;
      Resource  : Carthage.Resources.Resource_Type)
      return Boolean;

   function Harvest_Production
     (Structure : Structure_Record;
      Tile      : Carthage.Tiles.Tile_Type)
      return Production_Array
     with Pre => Structure.Is_Harvester;

   procedure Execute_Production
     (Structure : Structure_Record;
      Stock     : in out Carthage.Resources.Stock_Interface'Class;
      Factor    : Float);

   subtype Structure_Class is Structure_Record'Class;

   type Structure_Type is access constant Structure_Record'Class;

   function Bonus_Production
     (Structure       : Structure_Record;
      Bonus_Structure : Structure_Type)
      return Natural
     with Pre => Structure.Is_Harvester;

   function Exists (Id : String) return Boolean;

   function Get (Id : String) return Structure_Type
     with Pre => Exists (Id);

   function Get (Index : Positive) return Structure_Type;

private

   type Production_Record is
      record
         World    : Carthage.Worlds.World_Type;
         City     : Boolean;
         Terrain  : Carthage.Terrain.Terrain_Type;
         Resource : Carthage.Resources.Resource_Type;
         Quantity : Resource_Quantity;
      end record;

   package Production_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Production_Record);

   type Bonus_Production_Record is
      record
         Bonus_Structure : Structure_Type;
         Resource        : Carthage.Resources.Resource_Type;
         Quantity        : Natural;
      end record;

   package Bonus_Production_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Bonus_Production_Record);

   package Chance_Maps is
     new WL.String_Maps (Natural);

   type Structure_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with
      record
         Index          : Positive;
         Singular       : Boolean;
         Can_Build      : Boolean;
         Palace         : Boolean;
         Shield         : Boolean;
         Church         : Boolean;
         Agora          : Boolean;
         Water          : Boolean;
         Land           : Boolean;
         Road           : Boolean;
         Barren         : Boolean;
         Neutral        : Boolean;
         Is_Harvester   : Boolean;
         Is_Bonus       : Boolean;
         Area           : Natural;
         Maintenance    : Natural;
         Cost           : Natural;
         Build_Time     : Natural;
         Enabled_By     : Carthage.Technology.Technology_Type;
         Value          : Natural;
         Production     : Production_Lists.List;
         Inputs         : Production_Lists.List;
         Bonus          : Bonus_Production_Lists.List;
         Chances        : Chance_Maps.Map;
      end record;

   overriding function Object_Database
     (Item : Structure_Record)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       (Class_Name        => "structure",
        Element_Type      => Structure_Record,
        Element_Reference => Structure_Type);

   overriding function Object_Database
     (Item : Structure_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return Structure_Type
   is (Db.Get (Id));

   function Terrain_OK
     (Structure : Structure_Record;
      Terrain  : Carthage.Terrain.Terrain_Type)
      return Boolean
   is (not Terrain.Water or else Structure.Water);

   function Is_Harvester
     (Structure : Structure_Record)
      return Boolean
   is (Structure.Is_Harvester);

   function Is_Bonus
     (Structure : Structure_Record)
      return Boolean
   is (Structure.Is_Bonus);

   function Is_Agora
     (Structure : Structure_Record)
      return Boolean
   is (Structure.Agora);

   function Is_Palace
     (Structure : Structure_Record)
      return Boolean
   is (Structure.Palace);

   function Is_Shield
     (Structure : Structure_Record)
      return Boolean
   is (Structure.Shield);

   function Radius
     (Structure : Structure_Record)
      return Natural
   is (Structure.Area);

   package Structure_Vectors is
     new Ada.Containers.Vectors (Positive, Structure_Type);

   Fs : Structure_Vectors.Vector;

   function Get (Index : Positive) return Structure_Type
   is (Fs.Element (Index));

end Carthage.Structures;
