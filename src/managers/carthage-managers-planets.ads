limited with Carthage.Managers.Assets;
limited with Carthage.Managers.Cities;

with Carthage.Planets;
with Carthage.Tiles;

package Carthage.Managers.Planets is

   type Planet_Manager_Record is
     new Manager_Record
     and Carthage.Planets.Planet_Manager_Interface
   with private;

   procedure Add_Surface_Exploration_Goal
     (Manager : in out Planet_Manager_Record);

   subtype Planet_Manager_Class is Planet_Manager_Record'Class;

   type Planet_Manager_Type is access all Planet_Manager_Record'Class;

   function Create_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Planet_Manager_Type;

private

   package List_Of_Tiles is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Tiles.Tile_Type, Carthage.Tiles."=");

   type Tile_Info_Record is
      record
         Tile               : Carthage.Tiles.Tile_Type;
         Nearest_Seen       : Carthage.Tiles.Tile_Type;
         Nearest_Explored   : Carthage.Tiles.Tile_Type;
         Nearest_Controlled : Carthage.Tiles.Tile_Type;
         Interest           : Integer := 0;
         Controlled         : Boolean;
         Explored           : Boolean;
         Seen               : Boolean;
         Targeted           : Boolean;
      end record;

   type Tile_Info_Array is array (Tile_X, Tile_Y) of Tile_Info_Record;

   type Goal_Class is (Explore_Surface);

   type Planet_Manager_Goal is
     new Carthage.Goals.Goal_Record with
      record
         Class : Goal_Class;
      end record;

   function Default_Priority
     (Class : Goal_Class)
      return Carthage.Goals.Goal_Priority
   is (case Class is
          when Explore_Surface => 20);

   type Planet_Manager_Record is
     new Manager_Record
     and Carthage.Planets.Planet_Manager_Interface with
      record
         Planet               : Carthage.Planets.Planet_Type;
         Ground_Asset_Manager : access
           Carthage.Managers.Assets.Asset_Manager_Record'Class;
         City_Manager         : access
           Carthage.Managers.Cities.City_Manager_Record'Class;
         Controlled_Tiles     : List_Of_Tiles.List;
         Explored_Tiles       : List_Of_Tiles.List;
         Seen_Tiles           : List_Of_Tiles.List;
         Unseen_Tiles         : List_Of_Tiles.List;
         Target_Tiles         : List_Of_Tiles.List;
         Hostile_Tiles        : List_Of_Tiles.List;
         Active_Targets       : List_Of_Tiles.List;
         Tile_Info            : Tile_Info_Array;
      end record;

   overriding procedure Load_Initial_State
     (Manager : in out Planet_Manager_Record);

   overriding procedure Check_Goals
     (Manager : in out Planet_Manager_Record);

   overriding function Check_Goal
     (Manager : Planet_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   function Scan_Unexplored_Tiles
     (Manager : Planet_Manager_Record'Class)
      return Boolean;

end Carthage.Managers.Planets;
