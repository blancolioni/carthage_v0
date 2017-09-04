private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Memor.Database;

with Carthage.Assets;
with Carthage.Houses;
with Carthage.Planets;
with Carthage.Tiles;

with Carthage.Objects;

package Carthage.Stacks is

   Maximum_Stack_Size : constant := 20;

   type Asset_Count is range 0 .. Maximum_Stack_Size;
   subtype Asset_Index is Asset_Count range 1 .. Asset_Count'Last;

   type Stack_Record is
     new Carthage.Objects.Root_Named_Object with private;

   function Planet
     (Stack : Stack_Record)
      return Carthage.Planets.Planet_Type;

   function Has_Tile
     (Stack : Stack_Record)
      return Boolean;

   function In_Space
     (Stack : Stack_Record)
      return Boolean;

   function Tile
     (Stack : Stack_Record)
      return Carthage.Tiles.Tile_Type;

   function Owner
     (Stack : Stack_Record)
      return Carthage.Houses.House_Type;

   function Movement
     (Stack : Stack_Record)
      return Natural;

   function Count (Stack : Stack_Record) return Asset_Count;
   function Asset (Stack : Stack_Record;
                   Index : Asset_Index)
                   return Carthage.Assets.Asset_Type
     with Pre => Index <= Stack.Count;

   procedure Move_To_Tile
     (Stack : in out Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type);

   subtype Stack_Class is Stack_Record'Class;

   type Stack_Type is access constant Stack_Record'Class;

   procedure Add_Asset
     (To    : Stack_Type;
      Asset : Carthage.Assets.Asset_Type)
     with Pre => To.Count < Maximum_Stack_Size
       and then Carthage.Houses."=" (To.Owner, Asset.Owner);

   procedure Scan_Stacks
     (Process : not null access procedure (Stack : Stack_Type));

   procedure Update
     (Stack : Stack_Type;
      Update : not null access
        procedure (Rec : in out Stack_Class));

private

   type Asset_Array is array (Asset_Index) of Carthage.Assets.Asset_Type;

   type Stack_Order_Type is
     (Move_To_Tile,
      Move_To_Asset,
      Move_To_Planet);

   type Stack_Order_Record (Order_Type : Stack_Order_Type) is
      record
         case Order_Type is
            when Move_To_Tile =>
               Destination   : Tile_Position;
            when Move_To_Asset =>
               Target_Asset  : Carthage.Assets.Asset_Type;
            when Move_To_Planet =>
               Target_Planet : Carthage.Planets.Planet_Type;
         end case;
      end record;

   package Stack_Order_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Stack_Order_Record);

   type Stack_Record is
     new Carthage.Objects.Root_Named_Object with
      record
         Owner       : Carthage.Houses.House_Type;
         Planet      : Carthage.Planets.Planet_Type;
         Tile        : Carthage.Tiles.Tile_Type;
         Count       : Asset_Count;
         Assets      : Asset_Array;
         Orders      : Stack_Order_Lists.List;
      end record;

   overriding function Object_Database
     (Item : Stack_Record)
      return Memor.Memor_Database;

   overriding function Identifier
     (Item : Stack_Record)
      return String
   is (Item.Owner.Name & " stack size" & Asset_Count'Image (Item.Count)
       & (if Item.In_Space
          then " orbiting " & Item.Planet.Name
          else " at " & Carthage.Tiles.Position_Image (Item.Tile.Position)
          & " on " & Item.Planet.Name));

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

   function Has_Tile
     (Stack : Stack_Record)
      return Boolean
   is (Carthage.Tiles."/=" (Stack.Tile, null));

   function In_Space
     (Stack : Stack_Record)
      return Boolean
   is (not Stack.Has_Tile);

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
