with Carthage.Houses;
with Carthage.Planets;
with Carthage.Stacks;
with Carthage.Tiles;

package Carthage.Managers.Assets is

   type Asset_Meta_Manager_Interface is interface;

   procedure On_Hostile_Spotted
     (Manager : in out Asset_Meta_Manager_Interface;
      Spotter : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class)
   is null;

   function Ground_Asset_Manager
     (Meta_Manager : not null access Asset_Meta_Manager_Interface'Class;
      House        : Carthage.Houses.House_Type;
      Planet       : Carthage.Planets.Planet_Type)
      return Manager_Type;

   function Recon_Goal
     (Tile    : Carthage.Tiles.Tile_Type)
      return Carthage.Goals.Goal_Record'Class;

   function Capture_Goal
     (Tile     : Carthage.Tiles.Tile_Type;
      Strength : Natural)
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
         Tile       : Carthage.Tiles.Tile_Type;
         Parameters : Goal_Parameter_Record;
      end record;

   overriding function Show
     (Goal : Asset_Manager_Goal)
      return String
   is ("assets: " & Goal.Class'Img & " "
       & Carthage.Tiles.Position_Image (Goal.Tile.Position));

end Carthage.Managers.Assets;
