private with Memor.Database;

with Carthage.Objects;
with Carthage.Terrain;

with Carthage.Houses;

limited with Carthage.Cities;
limited with Carthage.Stacks;

package Carthage.Tiles is

   type Terrain_Layer is range 1 .. 3;

   function Position_Image (Position : Tile_Position) return String;

   type Tile_Record is
     new Carthage.Objects.Root_Carthage_Object with private;

   function Position
     (Tile : Tile_Record)
      return Tile_Position;

   function Terrain
     (Tile : Tile_Record;
      Layer : Terrain_Layer)
      return Carthage.Terrain.Terrain_Type;

   function Base_Terrain
     (Tile  : Tile_Record)
      return Carthage.Terrain.Terrain_Type
   is (Tile.Terrain (1));

   function Has_Terrain
     (Tile    : Tile_Record;
      Terrain : Carthage.Terrain.Terrain_Type)
      return Boolean;

   function Index
     (Tile : Tile_Record)
      return Positive;

   function Seen_By
     (Tile  : Tile_Record;
      House : Carthage.Houses.House_Type)
      return Boolean;

   function Explored_By
     (Tile  : Tile_Record;
      House : Carthage.Houses.House_Type)
      return Boolean;

   function Currently_Visible_To
     (Tile  : Tile_Record;
      House : Carthage.Houses.House_Type)
      return Boolean;

   function Is_Water (Tile : Tile_Record) return Boolean;

   function Has_City (Tile : Tile_Record) return Boolean;
   function Has_Road (Tile : Tile_Record) return Boolean;
   function Has_Stack (Tile : Tile_Record) return Boolean;

   function City (Tile : Tile_Record)
                  return access constant Carthage.Cities.City_Record'Class
     with Pre => Tile.Has_City;

   function Stack (Tile : Tile_Record)
                  return access constant Carthage.Stacks.Stack_Record'Class
     with Pre => Tile.Has_Stack;

   function Description
     (Tile : Tile_Record)
      return String;

   subtype Tile_Class is Tile_Record'Class;

   type Tile_Type is access constant Tile_Record'Class;

   procedure Set_City
     (Tile : Tile_Type;
      City : not null access constant Carthage.Cities.City_Record'Class);

   procedure Set_Stack
     (Tile  : Tile_Type;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class);

   procedure Set_Road
     (Tile : Tile_Type;
      Road : Boolean);

   procedure Set_Seen_By
     (Tile  : Tile_Type;
      House : Carthage.Houses.House_Type);

   procedure Set_Explored_By
     (Tile  : Tile_Type;
      House : Carthage.Houses.House_Type);

   procedure Set_Currently_Visible_To
     (Tile  : Tile_Type;
      House : Carthage.Houses.House_Type);

   procedure Clear_Visibility
     (Tile  : Tile_Type);

   --     procedure Update
--       (Tile : Tile_Type;
--        Update : not null access
--          procedure (Tile : in out Tile_Class));

private

   type Terrain_Layer_Array is
     array (Terrain_Layer) of Carthage.Terrain.Terrain_Type;

   type Tile_Record is
     new Carthage.Objects.Root_Carthage_Object with
      record
         Index     : Positive;
         Position  : Tile_Position;
         Height    : Integer;
         Seen      : Carthage.Houses.House_Set;
         Explored  : Carthage.Houses.House_Set;
         Visible   : Carthage.Houses.House_Set;
         Terrain   : Terrain_Layer_Array;
         Road      : Boolean;
         River     : Boolean;
         City      : access constant
           Carthage.Cities.City_Record'Class;
         Stack     : access constant
           Carthage.Stacks.Stack_Record'Class;
      end record;

   overriding function Object_Database
     (Item : Tile_Record)
      return Memor.Memor_Database;

   function Index
     (Tile : Tile_Record)
      return Positive
   is (Tile.Index);

   package Db is
     new Memor.Database
       (Class_Name        => "tile",
        Element_Type      => Tile_Record,
        Element_Reference => Tile_Type);

   overriding function Object_Database
     (Item : Tile_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Terrain
     (Tile  : Tile_Record;
      Layer : Terrain_Layer)
      return Carthage.Terrain.Terrain_Type
   is (Tile.Terrain (Layer));

   function Has_Terrain
     (Tile    : Tile_Record;
      Terrain : Carthage.Terrain.Terrain_Type)
      return Boolean
   is (for some Item of Tile.Terrain =>
          Carthage.Terrain."=" (Item, Terrain));

   function Is_Water (Tile : Tile_Record) return Boolean
   is (Tile.Terrain (1).Water);

   function Has_City (Tile : Tile_Record) return Boolean
   is (Tile.City /= null);

   function Has_Stack (Tile : Tile_Record) return Boolean
   is (Tile.Stack /= null);

   function Has_Road (Tile : Tile_Record) return Boolean
   is (Tile.Road);

   function City
     (Tile : Tile_Record)
      return access constant Carthage.Cities.City_Record'Class
   is (Tile.City);

   function Stack (Tile : Tile_Record)
                   return access constant Carthage.Stacks.Stack_Record'Class
   is (Tile.Stack);

   function Position
     (Tile : Tile_Record)
      return Tile_Position
   is (Tile.Position);

   function Seen_By
     (Tile  : Tile_Record;
      House : Carthage.Houses.House_Type)
      return Boolean
   is (Carthage.Houses.Element (Tile.Seen, House));

   function Explored_By
     (Tile  : Tile_Record;
      House : Carthage.Houses.House_Type)
      return Boolean
   is (Carthage.Houses.Element (Tile.Explored, House));

   function Currently_Visible_To
     (Tile  : Tile_Record;
      House : Carthage.Houses.House_Type)
      return Boolean
   is (Carthage.Houses.Element (Tile.Visible, House));

end Carthage.Tiles;
