private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with WL.String_Maps;

with Carthage.Assets;
with Carthage.Planets;
with Carthage.Resources;
with Carthage.Stacks;
with Carthage.Tiles;

package Carthage.Managers.Assets is

   type Asset_Meta_Manager_Interface is interface;

   procedure On_Hostile_Spotted
     (Manager : in out Asset_Meta_Manager_Interface;
      Spotter : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class)
   is null;

   type Asset_Manager_Record is
     new Manager_Record
     and Carthage.Stacks.Stack_Manager_Interface
   with private;

   overriding function Have_Immediate_Capacity
     (Manager : Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   function Recon_Goal
     (Manager : Asset_Manager_Record;
      Tile    : Carthage.Tiles.Tile_Type)
      return Carthage.Goals.Goal_Record'Class;

   function Capture_Goal
     (Manager  : Asset_Manager_Record;
      Tile     : Carthage.Tiles.Tile_Type;
      Strength : Natural)
      return Carthage.Goals.Goal_Record'Class;

   procedure Get_Resource_Requirements
     (Manager : in out Asset_Manager_Record;
      Minimum : in out Carthage.Resources.Stock_Interface'Class;
      Desired : in out Carthage.Resources.Stock_Interface'Class);

   procedure Transfer_Resources
     (Manager   : in out Asset_Manager_Record;
      Resources : in out Carthage.Resources.Stock_Interface'Class);

   subtype Asset_Manager_Class is Asset_Manager_Record'Class;

   type Asset_Manager_Type is access all Asset_Manager_Record'Class;

   function Create_Asset_Manager
     (Meta_Manager : not null access Asset_Meta_Manager_Interface'Class;
      House        : Carthage.Houses.House_Type;
      Planet       : Carthage.Planets.Planet_Type)
      return Asset_Manager_Type;

private

   type Goal_Class is (None, Recon, Capture);

   type Goal_Parameter is (Speed, Spot, Military);
   type Relative_Value is (Low, Medium, High);

   function Default_Priority
     (Class : Goal_Class)
      return Carthage.Goals.Goal_Priority
   is (case Class is
          when None => Carthage.Goals.Lowest_Priority,
          when Recon => 20,
          when Capture => 10);

   type Goal_Parameter_Record is
      record
         Speed     : Relative_Value := Low;
         Spot      : Relative_Value := Low;
         Military  : Relative_Value := Low;
         Strength  : Natural        := 0;
      end record;

   type Asset_Manager_Goal is
     new Carthage.Goals.Goal_Record with
      record
         Class      : Goal_Class := None;
         Tile       : Carthage.Tiles.Tile_Type;
         Parameters : Goal_Parameter_Record;
      end record;

   package Asset_Manager_Goal_Holders is
     new Ada.Containers.Indefinite_Holders (Asset_Manager_Goal);

   type Managed_Stack_Record is
      record
         Stack        : Carthage.Stacks.Stack_Type;
         Goal         : Asset_Manager_Goal_Holders.Holder;
         Minimum_Food : Natural := 0;
         Desired_Food : Natural := 0;
      end record;

   package Managed_Stack_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Stack_Record);

   package Managed_Stack_Maps is
     new WL.String_Maps (Managed_Stack_List.Cursor, Managed_Stack_List."=");

   type Managed_Asset_Record is
      record
         Asset : Carthage.Assets.Asset_Type;
         Stack : Managed_Stack_List.Cursor;
         Tile  : Carthage.Tiles.Tile_Type;
      end record;

   package Managed_Asset_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Asset_Record);

   package Asset_Classification_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Asset_List.Cursor, Managed_Asset_List."=");

   type Asset_Manager_Record is
     new Manager_Record
     and Carthage.Stacks.Stack_Manager_Interface with
      record
         Meta_Manager : access Asset_Meta_Manager_Interface'Class;
         Planet       : Carthage.Planets.Planet_Type;
         Assets       : Managed_Asset_List.List;
         Stacks       : Managed_Stack_List.List;
         Stack_Maps   : Managed_Stack_Maps.Map;
         Spotters     : Asset_Classification_List.List;
         Movers       : Asset_Classification_List.List;
         Minimum_Food : Natural;
         Desired_Food : Natural;
      end record;

   overriding procedure Load_Initial_State
     (Manager : not null access Asset_Manager_Record);

   overriding function Check_Goal
     (Manager : Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   overriding procedure Add_Goal
     (Manager : not null access Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class);

   overriding procedure On_Hostile_Spotted
     (Manager : in out Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class;
      Stop    : out Boolean);

   overriding procedure On_Stack_Removed
     (Manager : in out Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class);

end Carthage.Managers.Assets;
