with WL.Random;

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
          (Manager, House, Planet);
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

      procedure Scan_Area
        (Start : Tile_Position);

      ---------------
      -- Scan_Area --
      ---------------

      procedure Scan_Area
        (Start : Tile_Position)
      is
         Start_Tile : constant Carthage.Tiles.Tile_Type :=
                        Manager.Planet.Tile (Start);
         Class      : constant Planet_Area_Class :=
                        (if Start_Tile.Is_Water
                         then Sea else Continent);

         Border : array (Tile_X, Tile_Y) of Boolean :=
                    (others => (others => False));

         function Is_Member (Tile : Carthage.Tiles.Tile_Type) return Boolean
         is (case Class is
                when Sea => Tile.Is_Water,
                when Continent => not Tile.Is_Water);

         procedure Process (Tile : Carthage.Tiles.Tile_Type);

         -------------
         -- Process --
         -------------

         procedure Process (Tile : Carthage.Tiles.Tile_Type) is
            Ns           : constant Carthage.Planets.Array_Of_Tiles :=
                             Manager.Planet.Neighbour_Tiles (Tile.Position);
            Area         : Planet_Area_Record renames
                             Manager.Areas (Manager.Areas.Last);
            Found_Border : Boolean := False;
         begin
            Manager.Tile_Info (Tile.Position.X, Tile.Position.Y).Continent :=
              Manager.Areas.Last;
            Area.Tiles.Append (Tile);

            for N of Ns loop
               declare
                  X : constant Tile_X := N.Position.X;
                  Y : constant Tile_Y := N.Position.Y;
               begin
                  if not Border (X, Y) and then not Is_Member (N) then
                     Area.External_Border.Append (N);
                     Border (X, Y) := True;
                     Manager.Tile_Info (X, Y).Area_External_Border := True;
                     if not Found_Border then
                        Found_Border := True;
                        Area.Internal_Border.Append (Tile);
                        Manager.Tile_Info (Tile.Position.X, Tile.Position.Y)
                          .Area_Internal_Border := True;
                     end if;
                  end if;
               end;
            end loop;

         end Process;

      begin
         Manager.Areas.Append (Planet_Area_Record'(Class, others => <>));

         Manager.Planet.Scan_Connected_Tiles
           (Start   => Start,
            Test    => Is_Member'Access,
            Process => Process'Access);
      end Scan_Area;

   begin
      Manager.Owned :=
        Manager.Planet.Has_Owner
        and then Manager.Planet.Owner = Manager.House;

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
                          (Tile                 => Tile,
                           Nearest_Seen         => null,
                           Nearest_Explored     => null,
                           Nearest_Controlled   => null,
                           Interest             => 0,
                           Continent            =>
                             Planet_Area_Lists.No_Element,
                           Area_Internal_Border => False,
                           Area_External_Border => False,
                           Controlled           => False,
                           Explored             => False,
                           Seen                 => False,
                           Targeted             => False);

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
               end if;

               if Tile.Explored_By (Manager.House) then

                  if Tile.Has_City
                    and then Tile.City.Owner /= Manager.House
                    and then Manager.House.At_War_With (Tile.City.Owner)
                  then
                     Info.Interest := 600
                       + WL.Random.Random_Number (1, 100);
                  end if;

                  if Tile.Has_City then
                     if Tile.City.Structure.Is_Palace then
                        Manager.Palace := Tile.City;
                     elsif Tile.City.Structure.Is_Shield then
                        Manager.Shield := Tile.City;
                     end if;
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

      for Y in Tile_Y loop
         for X in Tile_X loop
            declare
               Info : Tile_Info_Record renames Manager.Tile_Info (X, Y);
            begin
               if not Planet_Area_Lists.Has_Element (Info.Continent) then
                  Scan_Area ((X, Y));
               end if;
            end;
         end loop;
      end loop;

      if Manager.Owned then
         declare
            Palace_Tile    : constant Carthage.Tiles.Tile_Type :=
                               Manager.Palace.Tile;
            Palace_Position : constant Tile_Position := Palace_Tile.Position;
            Home_Continent : constant Planet_Area_Lists.Cursor :=
                                Manager.Tile_Info
                                  (Palace_Position.X, Palace_Position.Y)
                                  .Continent;
         begin
            for Tile of Manager.Areas (Home_Continent).Internal_Border loop
               Manager.Tile_Info (Tile.Position.X, Tile.Position.Y).Interest :=
                 100 + WL.Random.Random_Number (1, 100);
            end loop;
         end;
      end if;

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

   ------------------------
   -- On_Hostile_Spotted --
   ------------------------

   overriding procedure On_Hostile_Spotted
     (Manager : in out Planet_Manager_Record;
      Spotter : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
      pragma Unreferenced (Spotter);
   begin
      if not Manager.Spotted_Hostiles.Contains (Hostile.Identifier) then
         Manager.Spotted_Hostiles.Insert (Hostile.Identifier, True);
         Manager.Ground_Asset_Manager.Add_Goal
           (Manager.Ground_Asset_Manager.Capture_Goal
              (Tile     => Hostile.Tile,
               Strength => Hostile.Total_Strength * 3));
      end if;
   end On_Hostile_Spotted;

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

      while not Tiles.Is_Empty loop

         declare
            Tile : constant Tile_Position := Tiles.First_Element;
            New_List : Tiles_Of_Interest_Lists.List;
         begin

            Manager.House.Log
              ("interest:"
               & Manager.Tile_Info (Tile.X, Tile.Y).Interest'Img
               & ": " & Manager.Planet.Tile (Tile).Description);
            declare
               Goal : constant Carthage.Goals.Goal_Record'Class :=
                        Manager.Ground_Asset_Manager.Recon_Goal
                          (Manager.Planet.Tile (Tile));
            begin
               if Manager.Ground_Asset_Manager.Have_Immediate_Capacity
                 (Goal)
               then
                  Manager.Ground_Asset_Manager.Add_Goal (Goal);
               else
                  exit;
               end if;
            end;

            for Check_Tile of Tiles loop
               if Carthage.Planets.Hex_Distance (Tile, Check_Tile) > 6 then
                  New_List.Append (Check_Tile);
               end if;
            end loop;

            Tiles := New_List;
         end;

      end loop;

      return True;

   end Scan_Unexplored_Tiles;

end Carthage.Managers.Planets;
