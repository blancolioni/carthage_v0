private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Memor.Database;

private with Carthage.Calendar;

with Carthage.Assets;
with Carthage.Houses;
with Carthage.Planets;
with Carthage.Resources;
with Carthage.Tiles;

with Carthage.Objects;

package Carthage.Stacks is

   Maximum_Stack_Size : constant := 20;

   type Stack_Manager_Interface is interface;

   procedure Take_Resource
     (From     : in out Stack_Manager_Interface;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : in out Resource_Quantity)
   is abstract;

   type Asset_Count is range 0 .. Maximum_Stack_Size;
   subtype Asset_Index is Asset_Count range 1 .. Asset_Count'Last;

   type Stack_Record is
     new Carthage.Objects.Root_Named_Object
     and Carthage.Assets.Asset_Container_Interface
   with private;

   overriding function Is_Empty (Stack : Stack_Record) return Boolean;
   overriding function Is_Full (Stack : Stack_Record) return Boolean;

   overriding function Has_Asset
     (Stack : Stack_Record;
      Asset : Carthage.Assets.Asset_Type)
      return Boolean;

   overriding procedure Add_Asset
     (To    : in out Stack_Record;
      Asset : Carthage.Assets.Asset_Type);

   overriding procedure Remove_Asset
     (From  : in out Stack_Record;
      Asset : Carthage.Assets.Asset_Type);

   overriding function First_Asset
     (Stack : Stack_Record)
      return Carthage.Assets.Asset_Type;

   function Description
     (Item : Stack_Record)
      return String;

   procedure On_Hostile_Spotted
     (Manager : in out Stack_Manager_Interface;
      Stack   : not null access constant Stack_Record'Class;
      Hostile : not null access constant Stack_Record'Class;
      Stop    : out Boolean)
   is abstract;

   procedure On_Movement_Ended
     (Manager : in out Stack_Manager_Interface;
      Stack   : not null access constant Stack_Record'Class)
   is null;

   procedure On_Stack_Removed
     (Manager : in out Stack_Manager_Interface;
      Stack   : not null access constant Stack_Record'Class)
   is null;

   procedure On_Asset_Added
     (Manager : in out Stack_Manager_Interface;
      Stack   : not null access constant Stack_Record'Class;
      Asset   : not null access constant Carthage.Assets.Asset_Record'Class)
   is null;

   procedure On_Asset_Removed
     (Manager : in out Stack_Manager_Interface;
      Stack   : not null access constant Stack_Record'Class;
      Asset   : not null access constant Carthage.Assets.Asset_Record'Class)
   is null;

   function Planet
     (Stack : Stack_Record)
      return Carthage.Planets.Planet_Type;

   function Has_Tile
     (Stack : Stack_Record)
      return Boolean;

   overriding function Is_Orbiting
     (Stack : Stack_Record)
      return Boolean;

   function Tile
     (Stack : Stack_Record)
      return Carthage.Tiles.Tile_Type;

   overriding function Owner
     (Stack : Stack_Record)
      return Carthage.Houses.House_Type;

   function Movement
     (Stack : Stack_Record)
      return Natural;

   function Has_Movement
     (Stack : Stack_Record)
      return Boolean;

   function Next_Tile
     (Stack : Stack_Record)
      return Tile_Position
     with Pre => Stack.Has_Movement;

   function Current_Movement
     (Stack : Stack_Record)
      return Array_Of_Positions
     with Pre => Stack.Has_Movement;

   function Spot
     (Stack : Stack_Record)
      return Natural;

   function Can_Enter
     (Stack : Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type)
      return Boolean;

   function Movement_Cost
     (Stack : Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type)
      return Natural;

   function Movement_Duration
     (Stack : Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type)
      return Duration
     with Pre => Stack.Can_Enter (Tile);

   function Movement_Progress
     (Stack : Stack_Record)
      return Float;

   function Find_Path
     (Stack   : Stack_Record'Class;
      Tile    : Carthage.Tiles.Tile_Type)
      return Array_Of_Positions;

   function Total
     (Stack : Stack_Record;
      Value : not null access
        function (Asset : Carthage.Assets.Asset_Type) return Integer)
      return Integer;

   function Total_Strength
     (Stack : Stack_Record'Class)
      return Natural;

   function Count (Stack : Stack_Record) return Asset_Count;
   function Asset (Stack : Stack_Record;
                   Index : Asset_Index)
                   return Carthage.Assets.Asset_Type
     with Pre => Index <= Stack.Count;

   procedure Move_To_Tile
     (Stack : in out Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type);

   procedure Remove_Dead_Assets
     (Stack : in out Stack_Record);

   procedure Set_Manager
     (Stack : in out Stack_Record;
      Manager : not null access Stack_Manager_Interface'Class);

   subtype Stack_Class is Stack_Record'Class;

   type Stack_Type is access constant Stack_Record'Class;

   procedure Scan_Stacks
     (Process : not null access procedure (Stack : Stack_Type));

   procedure Remove_Empty_Ground_Stacks;

   type Updateable_Reference (Item : not null access Stack_Record'Class)
   is private with Implicit_Dereference => Item;

   function Update
     (Item : not null access constant Stack_Record'Class)
      return Updateable_Reference;

   type Asset_Meta_Manager_Interface is interface;

   procedure On_Hostile_Spotted
     (Manager : in out Asset_Meta_Manager_Interface;
      Spotter : not null access constant Stack_Record'Class;
      Hostile : not null access constant Stack_Record'Class)
   is abstract;

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

   package Stack_Path_Holders is
     new Ada.Containers.Indefinite_Holders (Array_Of_Positions);

   type Stack_Record is
     new Carthage.Objects.Root_Named_Object
     and Carthage.Assets.Asset_Container_Interface with
      record
         Owner              : Carthage.Houses.House_Type;
         Planet             : Carthage.Planets.Planet_Type;
         Tile               : Carthage.Tiles.Tile_Type;
         Count              : Asset_Count := 0;
         Assets             : Asset_Array := (others => null);
         Orders             : Stack_Order_Lists.List;
         Current_Path       : Stack_Path_Holders.Holder;
         Current_Path_Index : Natural := 0;
         Next_Tile_Cost     : Positive := 1;
         Next_Tile_Duration : Duration;
         Next_Tile_Start    : Carthage.Calendar.Time;
         Manager            : access Stack_Manager_Interface'Class;
      end record;

   overriding function Object_Database
     (Item : Stack_Record)
      return Memor.Memor_Database;

   overriding function Variable_Reference
     (Stack : not null access constant Stack_Record)
      return access Carthage.Assets.Asset_Container_Interface'Class
   is (Stack.Update.Item);

   function Description
     (Item : Stack_Record)
      return String
   is (Item.Owner.Name & " stack size" & Asset_Count'Image (Item.Count)
       & (if Item.Is_Orbiting
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

   overriding function Is_Orbiting
     (Stack : Stack_Record)
      return Boolean
   is (not Stack.Has_Tile);

   overriding function Owner
     (Stack : Stack_Record)
      return Carthage.Houses.House_Type
   is (Stack.Owner);

   function Has_Movement
     (Stack : Stack_Record)
      return Boolean
   is (not Stack.Current_Path.Is_Empty);

   function Current_Movement
     (Stack : Stack_Record)
      return Array_Of_Positions
   is (Stack.Current_Path.Element);

   function Next_Tile
     (Stack : Stack_Record)
      return Tile_Position
   is (Stack.Current_Movement (Stack.Current_Path_Index));

   function Can_Enter
     (Stack : Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type)
      return Boolean
   is (Stack.Movement_Cost (Tile) > 0);

   overriding function Is_Empty (Stack : Stack_Record) return Boolean
   is (Stack.Count = 0);

   overriding function Is_Full (Stack : Stack_Record) return Boolean
   is (Stack.Count = Maximum_Stack_Size);

   function Count (Stack : Stack_Record) return Asset_Count
   is (Stack.Count);

   overriding function First_Asset
     (Stack : Stack_Record)
      return Carthage.Assets.Asset_Type
   is (Stack.Assets (1));

   overriding function Has_Asset (Stack : Stack_Record;
                       Asset : Carthage.Assets.Asset_Type)
                       return Boolean
   is (for some X of Stack.Assets (1 .. Stack.Count) =>
          Carthage.Assets."=" (X, Asset));

   function Asset (Stack : Stack_Record;
                   Index : Asset_Index)
                   return Carthage.Assets.Asset_Type
   is (Stack.Assets (Index));

   type Updateable_Reference (Item : not null access Stack_Record'Class) is
      record
         Update : Db.Updateable_Reference (Item);
      end record;

end Carthage.Stacks;
