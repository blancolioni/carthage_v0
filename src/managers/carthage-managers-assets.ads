private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;

private with WL.String_Maps;

private with Carthage.Calendar;
private with Carthage.Assets;

with Carthage.Houses;
with Carthage.Planets;
with Carthage.Stacks;
with Carthage.Tiles;

package Carthage.Managers.Assets is

   function Ground_Asset_Manager
     (Meta_Manager : not null access
        Carthage.Stacks.Asset_Meta_Manager_Interface'Class;
      House        : Carthage.Houses.House_Type;
      Planet       : Carthage.Planets.Planet_Type)
      return Manager_Type;

   function Space_Asset_Manager
     (Meta_Manager : not null access
        Carthage.Stacks.Asset_Meta_Manager_Interface'Class;
      House        : Carthage.Houses.House_Type)
      return Manager_Type;

   function Tile_Reconnaissance_Goal
     (Tile    : Carthage.Tiles.Tile_Type)
      return Carthage.Goals.Goal_Record'Class;

   function Tile_Capture_Goal
     (Tile     : Carthage.Tiles.Tile_Type;
      Strength : Natural)
      return Carthage.Goals.Goal_Record'Class;

   function Planet_Reconnaissance_Goal
     (Planet : Carthage.Planets.Planet_Type)
      return Carthage.Goals.Goal_Record'Class;

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
         Planet     : Carthage.Planets.Planet_Type;
         Tile       : Carthage.Tiles.Tile_Type;
         Parameters : Goal_Parameter_Record;
      end record;

   overriding function Show
     (Goal : Asset_Manager_Goal)
      return String
   is ("assets: " & Goal.Class'Img & " "
       & (if Carthage.Tiles."/=" (Goal.Tile, null)
          then Carthage.Tiles.Position_Image (Goal.Tile.Position)
          elsif Carthage.Planets."/=" (Goal.Planet, null)
          then Goal.Planet.Name
          else ""));

   package Asset_Manager_Goal_Holders is
     new Ada.Containers.Indefinite_Holders (Asset_Manager_Goal);

   type Managed_Stack_Record is
      record
         Stack        : Carthage.Stacks.Stack_Type;
         Goal         : Asset_Manager_Goal_Holders.Holder;
      end record;

   package Managed_Stack_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Stack_Record);

   package Managed_Stack_Maps is
     new WL.String_Maps (Managed_Stack_List.Cursor, Managed_Stack_List."=");

   type Managed_Asset_Record is
      record
         Asset  : Carthage.Assets.Asset_Type;
         Stack  : Managed_Stack_List.Cursor;
         Planet : Carthage.Planets.Planet_Type;
         Tile   : Carthage.Tiles.Tile_Type;
         Goal   : Asset_Manager_Goal_Holders.Holder;
      end record;

   package Managed_Asset_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Asset_Record);

   package Asset_Classification_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Asset_List.Cursor, Managed_Asset_List."=");

   type Root_Asset_Manager_Record is
     abstract new Root_Manager_Type
     and Carthage.Stacks.Stack_Manager_Interface with
      record
         Meta_Manager : access
           Carthage.Stacks.Asset_Meta_Manager_Interface'Class;
         Assets       : Managed_Asset_List.List;
         Spotters     : Asset_Classification_List.List;
         Movers       : Asset_Classification_List.List;
      end record;

   procedure Load_Assets
     (Manager : in out Root_Asset_Manager_Record)
   is abstract;

--     function Is_Idle
--       (Manager : Root_Asset_Manager_Record;
--        Asset   : Carthage.Assets.Asset_Type)
--        return Boolean
--        is abstract;
--
--     function Score
--       (Manager : Root_Asset_Manager_Record;
--        Asset   : Carthage.Assets.Asset_Type;
--        Goal    : Asset_Manager_Goal'Class)
--        return Boolean
--        is abstract;
--
   overriding procedure Initialize
     (Manager : in out Root_Asset_Manager_Record);

   overriding function Update
     (Manager : not null access Root_Asset_Manager_Record)
      return Duration;

   overriding procedure Get_Resource_Requirements
     (Manager : in out Root_Asset_Manager_Record;
      Minimum : in out Carthage.Resources.Stock_Interface'Class;
      Desired : in out Carthage.Resources.Stock_Interface'Class);

   overriding procedure Take_Resource
     (From     : in out Root_Asset_Manager_Record;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : in out Resource_Quantity);

   overriding function Average_Update_Frequency
     (Manager : Root_Asset_Manager_Record)
      return Duration
   is (Carthage.Calendar.Days (1));

end Carthage.Managers.Assets;
