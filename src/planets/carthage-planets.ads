private with Ada.Containers.Vectors;
private with WL.Graphs;
private with Memor.Database;
private with Memor.Element_Vectors;

with Carthage.Colours;

with Carthage.Objects.Localised;

with Carthage.Houses;
with Carthage.Worlds;
with Carthage.Tiles;

limited with Carthage.Stacks;

package Carthage.Planets is

   type Coordinate is new Float range 0.0 .. 1.0;

   type Array_Of_Positions is array (Positive range <>) of Tile_Position;

   type Planet_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with private;

   function Index
     (Planet : Planet_Record)
      return Positive;

   function X (Planet : Planet_Record) return Coordinate;
   function Y (Planet : Planet_Record) return Coordinate;

   function Category
     (Planet : Planet_Record)
      return Carthage.Worlds.World_Type;

   function Category_Name
     (Planet : Planet_Record)
      return String;

   function Tile_Set
     (Planet : Planet_Record)
      return String;

   function Tile (Planet   : Planet_Record;
                  Position : Tile_Position)
                  return Carthage.Tiles.Tile_Type;

   function Tile
     (Planet   : Planet_Record;
      X        : Tile_X;
      Y        : Tile_Y)
      return Carthage.Tiles.Tile_Type
   is (Planet.Tile ((X, Y)));

   function Road_Cost
     (Planet   : Planet_Record;
      Position : Tile_Position)
      return Natural;

   function Terrain_Colour
     (Planet   : Planet_Record;
      Position : Tile_Position)
      return Carthage.Colours.Colour_Type;

   function Neighbours
     (Planet   : Planet_Record;
      Position : Tile_Position)
      return Array_Of_Positions;

   function Hex_Distance
     (From, To : Tile_Position)
      return Natural;

   function Find_Path
     (Planet : Planet_Record;
      Start  : Tile_Position;
      Finish : Tile_Position;
      Cost   : not null access
        function (Tile : Carthage.Tiles.Tile_Type)
      return Float)
      return Array_Of_Positions;

   function Land_Connection
     (Planet   : Planet_Record;
      From, To : Tile_Position)
      return Boolean;

   type Array_Of_Tiles is
     array (Positive range <>) of Carthage.Tiles.Tile_Type;

   function Neighbour_Tiles
     (Planet   : Planet_Record;
      Position : Tile_Position)
      return Array_Of_Tiles;

   procedure Scan_Connected_Tiles
     (Planet  : Planet_Record;
      Start   : Tile_Position;
      Test    : not null access
        function (Tile : Carthage.Tiles.Tile_Type) return Boolean;
      Process : not null access
        procedure (Tile : Carthage.Tiles.Tile_Type));

   function Find_Tile
     (Planet : Planet_Record;
      Start  : Tile_Position;
      Test   : not null access
        function (Position : Tile_Position) return Boolean)
      return Tile_Position;

   function Has_Owner (Planet : Planet_Record) return Boolean;

   function Owner (Planet : Planet_Record) return Carthage.Houses.House_Type
     with Pre => Planet.Has_Owner;

   procedure Set_Owner
     (Planet : in out Planet_Record;
      New_Owner : Carthage.Houses.House_Type);

   function Seen_By
     (Planet : Planet_Record;
      House  : Carthage.Houses.House_Type)
      return Boolean;

   type Surface_Tiles is private;

   procedure Get_Tiles (Planet : not null access constant Planet_Record'Class;
                        Tiles  : out Surface_Tiles);

   procedure Get_Tiles (Planet : not null access constant Planet_Record'Class;
                        Test   : not null access
                          function (Tile : Carthage.Tiles.Tile_Type)
                          return Boolean;
                        Tiles  : out Surface_Tiles);

   procedure Get_Tiles
     (Planet       : not null access constant Planet_Record'Class;
      Origin       : Carthage.Tiles.Tile_Type;
      Min_Distance : Natural;
      Max_Distance : Natural;
      Test         : access
        function (Tile : Carthage.Tiles.Tile_Type)
      return Boolean;
      Tiles        : out Surface_Tiles);

   procedure Remove_Tile
     (Tiles : in out Surface_Tiles;
      Position : Tile_Position);

   procedure Remove_Tiles
     (Tiles    : in out Surface_Tiles;
      Test     : not null access
        function (Tile : Carthage.Tiles.Tile_Type) return Boolean);

   procedure Remove_Tiles
     (Tiles        : in out Surface_Tiles;
      Position     : Tile_Position;
      Max_Distance : Natural);

   function Tile_Count (Tiles : Surface_Tiles) return Natural;
   function Get_Tile (Tiles : Surface_Tiles;
                      Index : Positive)
                      return Carthage.Tiles.Tile_Type;

   function Stack
     (Planet : Planet_Record;
      House  : Carthage.Houses.House_Type)
      return access constant Carthage.Stacks.Stack_Record'Class;

   subtype Planet_Class is Planet_Record'Class;

   type Planet_Type is access constant Planet_Record'Class;

   function Exists (Id : String) return Boolean;
   function Get (Id : String) return Planet_Type
     with Pre => Exists (Id);

   procedure Clear_Visibility (Planet : in out Planet_Class);

   procedure Scan
     (Process : not null access procedure (Planet : Planet_Type));

   procedure Update
     (Planet : Planet_Type;
      Update : not null access
        procedure (Planet : in out Planet_Class));

   procedure Set_Seen_By
     (Planet : Planet_Type;
      House  : Carthage.Houses.House_Type);

   function Number_Of_Planets return Natural;

private

   type Tile_Array is
     array (Tile_X, Tile_Y) of Carthage.Tiles.Tile_Type;

   function Index_Of (Tile : Carthage.Tiles.Tile_Type) return Positive
   is (Tile.Index);

   function Index_Of (Position : Tile_Position) return Positive
   is (Positive (Position.X) + Planet_Width * (Natural (Position.Y) - 1));

   package Tile_Graphs is
     new WL.Graphs
       (Index_Type   => Positive,
        Vertex_Type  => Tile_Position,
        Cost_Type    => Float,
        Default_Cost => 1.0,
        Index_Of     => Index_Of);

   Surface_Graph : Tile_Graphs.Graph;

   Orbital_Stack_Count : constant := 8;

   type Orbital_Stack_Type is
     access constant Carthage.Stacks.Stack_Record'Class;

   package Orbital_Stack_Vectors is
     new Memor.Element_Vectors
       (Carthage.Houses.House_Record, Orbital_Stack_Type, null);

   type Planet_Record is
     new Carthage.Objects.Localised.Root_Localised_Object with
      record
         Index    : Positive;
         X, Y     : Coordinate;
         Category : Carthage.Worlds.World_Type;
         Seen     : Carthage.Houses.House_Set;
         Tiles    : Tile_Array;
         Megacity : Boolean;
         Owner    : Carthage.Houses.House_Type;
         Stacks   : Orbital_Stack_Vectors.Vector;
      end record;

   overriding function Object_Database
     (Item : Planet_Record)
      return Memor.Memor_Database;

   function Index
     (Planet : Planet_Record)
      return Positive
   is (Planet.Index);

   function X (Planet : Planet_Record) return Coordinate
   is (Planet.X);

   function Y (Planet : Planet_Record) return Coordinate
   is (Planet.Y);

   package Db is
     new Memor.Database
       (Class_Name        => "planet",
        Element_Type      => Planet_Record,
        Element_Reference => Planet_Type);

   overriding function Object_Database
     (Item : Planet_Record)
      return Memor.Memor_Database
   is (Db.Get_Database);

   function Exists (Id : String) return Boolean
   is (Db.Exists (Id));

   function Get (Id : String) return Planet_Type
   is (Db.Get (Id));

   function Number_Of_Planets return Natural
   is (Db.Active_Count);

   function Has_Owner (Planet : Planet_Record) return Boolean
   is (Carthage.Houses."/=" (Planet.Owner, null));

   function Owner (Planet : Planet_Record) return Carthage.Houses.House_Type
   is (Planet.Owner);

   function Category
     (Planet : Planet_Record)
      return Carthage.Worlds.World_Type
   is (Planet.Category);

   function Tile (Planet   : Planet_Record;
                  Position : Tile_Position)
                  return Carthage.Tiles.Tile_Type
   is (Planet.Tiles (Position.X, Position.Y));

   package Tile_Vectors is
     new Ada.Containers.Vectors (Positive, Carthage.Tiles.Tile_Type,
                                 Carthage.Tiles."=");

   type Surface_Tiles is
      record
         Planet : Planet_Type;
         Tiles  : Tile_Vectors.Vector;
      end record;

   function Tile_Count (Tiles : Surface_Tiles) return Natural
   is (Tiles.Tiles.Last_Index);

   function Get_Tile (Tiles : Surface_Tiles;
                      Index : Positive)
                      return Carthage.Tiles.Tile_Type
   is (Tiles.Tiles.Element (Index));

   function Category_Name
     (Planet : Planet_Record)
      return String
   is (if Planet.Megacity then "city" else Planet.Category.Identifier);

   function Terrain_Colour
     (Planet   : Planet_Record;
      Position : Tile_Position)
      return Carthage.Colours.Colour_Type
   is (Planet.Tile (Position).Terrain (1).Colour (Planet.Category_Name));

   function Seen_By
     (Planet : Planet_Record;
      House  : Carthage.Houses.House_Type)
      return Boolean
   is (Carthage.Houses.Element (Planet.Seen, House));

end Carthage.Planets;
