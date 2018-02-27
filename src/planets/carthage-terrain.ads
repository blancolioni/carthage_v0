private with WL.String_Maps;

private with Memor.Database;

with Carthage.Colors;
with Carthage.Objects.Localised;

package Carthage.Terrain is

   type Terrain_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Water
     (Terrain : Terrain_Record)
      return Boolean;

   function Ocean
     (Terrain : Terrain_Record)
      return Boolean;

   function Color
     (Terrain  : Terrain_Record;
      Category : String)
      return Carthage.Colors.Color_Type;

   function Road_Cost
     (Terrain  : Terrain_Record;
      Category : String)
      return Natural;

   subtype Terrain_Class is Terrain_Record'Class;

   type Terrain_Type is access constant Terrain_Record'Class;

   function Exists (Id : String) return Boolean;
   function Get (Id : String) return Terrain_Type
     with Pre => Exists (Id);

   function Terrain_Count return Natural;

   procedure Scan
     (Process : not null access procedure (Terrain : Terrain_Type));

private

   type Terrain_Category_Info is
      record
         Color : Carthage.Colors.Color_Type;
         Road_Cost : Natural;
      end record;

   package Terrain_Category_Maps is
     new WL.String_Maps (Terrain_Category_Info);

   type Terrain_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with
      record
         Water     : Boolean;
         Ocean     : Boolean;
         Cat_Info  : Terrain_Category_Maps.Map;
      end record;

   overriding function Object_Database
     (Item : Terrain_Record)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       (Class_Name        => "terrain",
        Element_Type      => Terrain_Record,
        Element_Reference => Terrain_Type);

   overriding function Object_Database
     (Item : Terrain_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return Terrain_Type
   is (Db.Get (Id));

   function Water
     (Terrain : Terrain_Record)
      return Boolean
   is (Terrain.Water);

   function Ocean
     (Terrain : Terrain_Record)
      return Boolean
   is (Terrain.Ocean);

   function Color
     (Terrain  : Terrain_Record;
      Category : String)
      return Carthage.Colors.Color_Type
   is (Terrain.Cat_Info.Element (Category).Color);

   function Road_Cost
     (Terrain  : Terrain_Record;
      Category : String)
      return Natural
   is (Terrain.Cat_Info.Element (Category).Road_Cost);

end Carthage.Terrain;
