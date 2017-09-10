with WL.Random;

with Carthage.Stacks;

with Carthage.Managers.Assets;
with Carthage.Managers.Cities;

package body Carthage.Managers.Planets is

   package Tiles_Of_Interest_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Tile_Position);

   ----------------------------------
   -- Add_Surface_Exploration_Goal --
   ----------------------------------

   procedure Add_Surface_Exploration_Goal
     (Manager : in out Planet_Manager_Record)
   is
   begin
      Manager.Goals.Append
        (Planet_Manager_Goal'
           (Carthage.Goals.Goal_Record with
                Priority   => Default_Priority (Explore_Surface),
            Class      => Explore_Surface));
   end Add_Surface_Exploration_Goal;

   ----------------
   -- Check_Goal --
   ----------------

   overriding function Check_Goal
     (Manager : Planet_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      Planet_Goal : Planet_Manager_Goal renames Planet_Manager_Goal (Goal);
   begin
      case Planet_Goal.Class is
         when Explore_Surface =>
            return Manager.Scan_Unexplored_Tiles;
      end case;
   end Check_Goal;

   -----------------
   -- Check_Goals --
   -----------------

   overriding procedure Check_Goals
     (Manager : in out Planet_Manager_Record)
   is
   begin
      Manager_Record (Manager).Check_Goals;
      Manager.Ground_Asset_Manager.Check_Goals;
      Manager.City_Manager.Check_Goals;
   end Check_Goals;

   ---------------------------
   -- Create_Planet_Manager --
   ---------------------------

   function Create_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Planet_Manager_Type
   is
      Manager : constant Planet_Manager_Type :=
                  new Planet_Manager_Record;
   begin
      Manager.House := House;
      Manager.Planet := Planet;
      Manager.Ground_Asset_Manager :=
        Carthage.Managers.Assets.Create_Asset_Manager
          (House, Planet);
      Manager.City_Manager :=
        Carthage.Managers.Cities.Create_City_Manager
          (House, Planet);
      return Manager;
   end Create_Planet_Manager;

   ------------------
   -- Execute_Turn --
   ------------------

   overriding procedure Execute_Turn
     (Manager : in out Planet_Manager_Record)
   is
      Minimum_Stock   : Carthage.Resources.Stock_Record;
      Desired_Stock   : Carthage.Resources.Stock_Record;
      Available_Stock : Carthage.Resources.Stock_Record;
   begin
      Manager_Record (Manager).Execute_Turn;
      Manager.Ground_Asset_Manager.Get_Resource_Requirements
        (Minimum_Stock, Desired_Stock);
      Manager.City_Manager.Set_Resource_Requirements
        (Minimum => Minimum_Stock,
         Desired => Desired_Stock,
         Result  => Available_Stock);
      Manager.Ground_Asset_Manager.Transfer_Resources
        (Available_Stock);

      Manager.Ground_Asset_Manager.Execute_Turn;
      Manager.City_Manager.Execute_Turn;
   end Execute_Turn;

   ------------------------
   -- Load_Initial_State --
   ------------------------

   overriding procedure Load_Initial_State
     (Manager : not null access Planet_Manager_Record)
   is
      use type Carthage.Houses.House_Type;
   begin
      Manager.Controlled_Tiles.Clear;
      Manager.Explored_Tiles.Clear;
      Manager.Seen_Tiles.Clear;
      Manager.Unseen_Tiles.Clear;
      Manager.Hostile_Tiles.Clear;

      for Y in Tile_Y loop
         for X in Tile_X loop
            declare
               Tile : constant Carthage.Tiles.Tile_Type :=
                        Manager.Planet.Tile (X, Y);
               Info : Tile_Info_Record :=
                        Tile_Info_Record'
                          (Tile               => Tile,
                           Nearest_Seen       => null,
                           Nearest_Explored   => null,
                           Nearest_Controlled => null,
                           Interest           => 0,
                           Controlled         => False,
                           Explored           => False,
                           Seen               => False,
                           Targeted           => False);
               function At_War_With
                 (Enemy : not null access constant
                    Carthage.Stacks.Stack_Record'Class)
                  return Boolean
               is (Manager.House.At_War_With (Enemy.Owner));

            begin
               if Tile.Currently_Visible_To (Manager.House) then

                  if Tile.Has_Stack (At_War_With'Access) then
                     Manager.Planet.Log
                       (Manager.House.Name
                        & ": hostile at "
                        & Carthage.Tiles.Position_Image (Tile.Position));
                     Info.Interest := 1_000
                       + Natural (Tile.Find_Stack (At_War_With'Access).Count)
                       + WL.Random.Random_Number (1, 100);
                     Manager.Hostile_Tiles.Append (Tile);
                  end if;

                  Manager.Controlled_Tiles.Append (Tile);
                  Info.Nearest_Seen := Tile;
                  Info.Nearest_Explored := Tile;
                  Info.Nearest_Controlled := Tile;
               elsif Tile.Explored_By (Manager.House) then

                  if Tile.Has_City
                    and then Tile.City.Owner /= Manager.House
                    and then Manager.House.At_War_With (Tile.City.Owner)
                  then
                     Info.Interest := 600
                       + WL.Random.Random_Number (1, 100);
                  end if;

                  Manager.Explored_Tiles.Append (Tile);
                  Info.Nearest_Explored := Tile;
                  Info.Nearest_Controlled := Tile;

               elsif Tile.Seen_By (Manager.House) then
                  Manager.Seen_Tiles.Append (Tile);
                  Info.Nearest_Seen := Tile;
                  Info.Interest := WL.Random.Random_Number (1, 100);
               else
                  Manager.Unseen_Tiles.Append (Tile);
                  declare
                     use Carthage.Planets;
                     Ns : Surface_Tiles;
                  begin
                     Manager.Planet.Get_Tiles
                       (Tile, 1, 3, null, Ns);
                     Info.Interest := 100;
                     for I in 1 .. Tile_Count (Ns) loop
                        if Get_Tile (Ns, I).Seen_By (Manager.House) then
                           Info.Interest := Info.Interest + 6
                             + WL.Random.Random_Number (1, 6);
                        end if;
                     end loop;
                  end;

               end if;
               Manager.Tile_Info (X, Y) := Info;
            end;
         end loop;
      end loop;

      Manager.Ground_Asset_Manager.Load_Initial_State;
      Manager.City_Manager.Load_Initial_State;
   end Load_Initial_State;

   -------------
   -- Execute --
   -------------

--     overriding procedure Execute
--       (Manager : in out Planet_Manager_Record)
--     is
--     begin
--        for Tile of Manager.Active_Targets loop
--           if Tile.Has_Stack
--             and then Manager.House.At_War_With (Tile.Stack.Owner)
--           then
--              Manager.Ground_Asset_Manager.Add_Request
--                (Tile_Attack_Request
--                   (Manager.Planet, Tile));
--           else
--              Manager.Ground_Asset_Manager.Add_Request
--                (Tile_Recon_Request
--                   (Manager.Planet, Tile));
--           end if;
--        end loop;
--
--        Manager.Ground_Asset_Manager.Execute;
--        Manager.City_Manager.Execute;
--     end Execute;

   ---------------------------
   -- Scan_Unexplored_Tiles --
   ---------------------------

   function Scan_Unexplored_Tiles
     (Manager : Planet_Manager_Record'Class)
      return Boolean
   is
      Tiles : Tiles_Of_Interest_Lists.List;

      function Higher_Interest (Left, Right : Tile_Position) return Boolean
      is (Manager.Tile_Info (Left.X, Left.Y).Interest
          > Manager.Tile_Info (Right.X, Right.Y).Interest);

      package Sorting is
        new Tiles_Of_Interest_Lists.Generic_Sorting
          (Higher_Interest);

   begin
      Manager.Planet.Log
        (Manager.House.Name & ": looking for unexplored tiles");

      for Unseen of Manager.Unseen_Tiles loop
         Tiles.Append (Unseen.Position);
      end loop;
      for Seen of Manager.Seen_Tiles loop
         Tiles.Append (Seen.Position);
      end loop;

      Manager.Planet.Log
        (Manager.House.Name & ":" & Natural'Image (Natural (Tiles.Length))
         & " tiles found");

      Sorting.Sort (Tiles);

      for Tile of Tiles loop
         Manager.House.Log
           ("interest:"
            & Manager.Tile_Info (Tile.X, Tile.Y).Interest'Img
            & ": " & Manager.Planet.Tile (Tile).Description);
         declare
            Goal : constant Carthage.Goals.Goal_Record'Class :=
                     Manager.Ground_Asset_Manager.Recon_Goal
                       (Manager.Planet.Tile (Tile));
         begin
            if Manager.Ground_Asset_Manager.Have_Immediate_Capacity (Goal) then
               Manager.Ground_Asset_Manager.Add_Goal (Goal);
            else
               exit;
            end if;
         end;
      end loop;

      return True;

   end Scan_Unexplored_Tiles;

end Carthage.Managers.Planets;
