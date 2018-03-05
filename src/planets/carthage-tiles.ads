private with Ada.Containers.Doubly_Linked_Lists;
private with Memor.Database;

with Carthage.Objects;
with Carthage.Terrain;

with Carthage.Houses;

limited with Carthage.Cities;
limited with Carthage.Stacks;

package Carthage.Tiles is

   type Terrain_Layer is range 1 .. 3;

   type Terrain_Layer_Array is
     array (Terrain_Layer range <>) of Carthage.Terrain.Terrain_Type;

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

   function Terrain
     (Tile  : Tile_Record)
      return Terrain_Layer_Array;

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
   function Has_Stacks (Tile : Tile_Record) return Boolean;

   function City (Tile : Tile_Record)
                  return access constant Carthage.Cities.City_Record'Class
     with Pre => Tile.Has_City;

   function First_Stack
     (Tile : Tile_Record)
      return access constant Carthage.Stacks.Stack_Record'Class
     with Pre => Tile.Has_Stacks;

   procedure Scan_Stacks
     (Tile          : Tile_Record;
      Process       : not null access
        procedure (Stack : not null access constant
                     Carthage.Stacks.Stack_Record'Class);
      Skip_Empty    : Boolean := True);

   function Description
     (Tile : Tile_Record)
      return String;

   procedure Set_City
     (Tile : in out Tile_Record;
      City : not null access constant Carthage.Cities.City_Record'Class);

   function Has_Stack
     (Tile  : Tile_Record;
      Stack : not null access constant
        Carthage.Stacks.Stack_Record'Class)
      return Boolean;

   procedure Add_Stack
     (Tile  : in out Tile_Record;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class)
     with Pre => not Tile.Has_Stack (Stack),
     Post => Tile.Has_Stack (Stack);

   procedure Remove_Stack
     (Tile  : in out Tile_Record;
      Stack : not null access constant Carthage.Stacks.Stack_Record'Class)
     with Pre => Tile.Has_Stack (Stack),
     Post => not Tile.Has_Stack (Stack);

   procedure Remove_Stacks
     (Tile  : in out Tile_Record;
      Match : not null access
        function (Stack : not null access constant
                    Carthage.Stacks.Stack_Record'Class)
      return Boolean);

   function Has_Stack
     (Tile  : Tile_Record;
      Match : not null access
        function (Stack : not null access constant
                    Carthage.Stacks.Stack_Record'Class)
      return Boolean)
      return Boolean;

   function Find_Stack
     (Tile  : Tile_Record;
      Match : not null access
        function (Stack : not null access constant
                    Carthage.Stacks.Stack_Record'Class)
      return Boolean)
      return access constant Carthage.Stacks.Stack_Record'Class;

   procedure Set_Road
     (Tile : in out Tile_Record;
      Road : Boolean);

   procedure Set_Seen_By
     (Tile  : in out Tile_Record;
      House : Carthage.Houses.House_Type);

   procedure Set_Explored_By
     (Tile  : in out Tile_Record;
      House : Carthage.Houses.House_Type);

   procedure Set_Currently_Visible_To
     (Tile  : in out Tile_Record;
      House : Carthage.Houses.House_Type);

   procedure Clear_Visibility
     (Tile : in out Tile_Record);

   subtype Tile_Class is Tile_Record'Class;

   type Tile_Type is access constant Tile_Record'Class;

   type Array_Of_Tiles is array (Positive range <>) of Tile_Type;

   type Updateable_Reference (Item : not null access Tile_Record'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Tile_Record'Class)
      return Updateable_Reference;

private

   type Stack_Access is access constant Carthage.Stacks.Stack_Record'Class;

   package Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stack_Access);

   type Tile_Record is
     new Carthage.Objects.Root_Carthage_Object with
      record
         Index     : Positive;
         Position  : Tile_Position;
         Height    : Integer;
         Seen      : Carthage.Houses.House_Set;
         Explored  : Carthage.Houses.House_Set;
         Visible   : Carthage.Houses.House_Set;
         Terrain   : Terrain_Layer_Array
           (Terrain_Layer'First .. Terrain_Layer'Last);
         Road      : Boolean;
         River     : Boolean;
         City      : access constant
           Carthage.Cities.City_Record'Class;
         Stacks    : Stack_Lists.List;
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

   function Has_Stack
     (Tile  : Tile_Record;
      Stack : not null access constant
        Carthage.Stacks.Stack_Record'Class)
      return Boolean
   is (for some S of Tile.Stacks =>
          S = Stack_Access (Stack));

   function Has_Stack
     (Tile  : Tile_Record;
      Match : not null access
        function (Stack : not null access constant
                    Carthage.Stacks.Stack_Record'Class)
      return Boolean)
      return Boolean
   is (for some S of Tile.Stacks => Match (S));

   function Is_Water (Tile : Tile_Record) return Boolean
   is (Tile.Terrain (1).Water);

   function Has_City (Tile : Tile_Record) return Boolean
   is (Tile.City /= null);

   function Has_Stacks (Tile : Tile_Record) return Boolean
   is (not Tile.Stacks.Is_Empty);

   function Has_Road (Tile : Tile_Record) return Boolean
   is (Tile.Road);

   function City
     (Tile : Tile_Record)
      return access constant Carthage.Cities.City_Record'Class
   is (Tile.City);

   function First_Stack
     (Tile : Tile_Record)
      return access constant Carthage.Stacks.Stack_Record'Class
   is (Tile.Stacks.First_Element);

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

   type Updateable_Reference (Item : not null access Tile_Record'Class) is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Carthage.Tiles;
