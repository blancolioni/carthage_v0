private with Memor.Database;

with Carthage.Assets;
with Carthage.Houses;
with Carthage.Planets;
with Carthage.Tiles;

with Carthage.Objects;

package Carthage.Stacks is

   Maximum_Stack_Size : constant := 16;

   type Asset_Count is range 0 .. Maximum_Stack_Size;
   subtype Asset_Index is Asset_Count range 1 .. Asset_Count'Last;

   type Stack_Record is
     new Carthage.Objects.Root_Named_Object with private;

   function Planet
     (Stack : Stack_Record)
      return Carthage.Planets.Planet_Type;

   function Tile
     (Stack : Stack_Record)
      return Carthage.Tiles.Tile_Type;

   function Owner
     (Stack : Stack_Record)
      return Carthage.Houses.House_Type;

   function Count (Stack : Stack_Record) return Asset_Count;
   function Asset (Stack : Stack_Record;
                   Index : Asset_Index)
                   return Carthage.Assets.Asset_Type
     with Pre => Index <= Stack.Count;

   subtype Stack_Class is Stack_Record'Class;

   type Stack_Type is access constant Stack_Record'Class;

   procedure Add_Asset
     (To    : Stack_Type;
      Asset : Carthage.Assets.Asset_Type)
     with Pre => To.Count < Maximum_Stack_Size
       and then Carthage.Houses."=" (To.Owner, Asset.Owner);

private

   type Asset_Array is array (Asset_Index) of Carthage.Assets.Asset_Type;

   type Stack_Record is
     new Carthage.Objects.Root_Named_Object with
      record
         Owner       : Carthage.Houses.House_Type;
         Planet      : Carthage.Planets.Planet_Type;
         Tile        : Carthage.Tiles.Tile_Type;
         Count       : Asset_Count;
         Assets      : Asset_Array;
      end record;

   overriding function Object_Database
     (Item : Stack_Record)
      return Memor.Memor_Database;

   package Db is
     new Memor.Database
       (Class_Name        => "stack",
        Element_Type      => Stack_Record,
        Element_Reference => Stack_Type);

   overriding function Object_Database
     (Item : Stack_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Planet
     (Stack : Stack_Record)
      return Carthage.Planets.Planet_Type
   is (Stack.Planet);

   function Tile
     (Stack : Stack_Record)
      return Carthage.Tiles.Tile_Type
   is (Stack.Tile);

   function Owner
     (Stack : Stack_Record)
      return Carthage.Houses.House_Type
   is (Stack.Owner);

   function Count (Stack : Stack_Record) return Asset_Count
   is (Stack.Count);

   function Asset (Stack : Stack_Record;
                   Index : Asset_Index)
                   return Carthage.Assets.Asset_Type
   is (Stack.Assets (Index));

end Carthage.Stacks;
