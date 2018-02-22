with Ada.Containers.Doubly_Linked_Lists;

with WL.Random;
with WL.String_Maps;

with Carthage.Calendar;

with Carthage.Cities;
with Carthage.Stacks;
with Carthage.Tiles;

with Carthage.Managers.Assets;
with Carthage.Managers.Cities;

package body Carthage.Managers.Planets is

   package List_Of_Tiles is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Tiles.Tile_Type, Carthage.Tiles."=");

   package Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Stacks.Stack_Type, Carthage.Stacks."=");

   type Planet_Area_Class is (Sea, Continent);

   type Planet_Area_Record is
      record
         Class           : Planet_Area_Class;
         Tiles           : List_Of_Tiles.List;
         Internal_Border : List_Of_Tiles.List;
         External_Border : List_Of_Tiles.List;
      end record;

   package Planet_Area_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Planet_Area_Record);

   type Tile_Info_Record is
      record
         Tile                 : Carthage.Tiles.Tile_Type;
         Nearest_Seen         : Carthage.Tiles.Tile_Type;
         Nearest_Explored     : Carthage.Tiles.Tile_Type;
         Nearest_Controlled   : Carthage.Tiles.Tile_Type;
         Interest             : Integer := 0;
         Continent            : Planet_Area_Lists.Cursor :=
                                  Planet_Area_Lists.No_Element;
         Area_Internal_Border : Boolean;
         Area_External_Border : Boolean;
         Controlled           : Boolean;
         Explored             : Boolean;
         Seen                 : Boolean;
         Targeted             : Boolean;
      end record;

   type Tile_Info_Array is array (Tile_X, Tile_Y) of Tile_Info_Record;

   type Goal_Class is (Explore_Surface);

--     type Planet_Manager_Goal is
--       new Carthage.Goals.Goal_Record with
--        record
--           Class : Goal_Class;
--        end record;
--
--     overriding function Show
--       (Goal : Planet_Manager_Goal)
--        return String
--     is ("planet goal: " & Goal.Class'Img);

   function Default_Priority
     (Class : Goal_Class)
      return Carthage.Goals.Goal_Priority
   is (case Class is
          when Explore_Surface => 20)
        with Unreferenced;

   package Identifier_Sets is
     new WL.String_Maps (Boolean);

   package City_Manager_Maps is
     new WL.String_Maps (Manager_Type);

   type Planet_Manager_Record is
     new Root_Manager_Type
     and Carthage.Stacks.Asset_Meta_Manager_Interface with
      record
         Owned                : Boolean;
         Active               : Boolean;
         House                : Carthage.Houses.House_Type;
         Planet               : Carthage.Planets.Planet_Type;
         City_Managers        : City_Manager_Maps.Map;
         Ground_Asset_Manager : Manager_Type;
         Areas                : Planet_Area_Lists.List;
         Controlled_Tiles     : List_Of_Tiles.List;
         Explored_Tiles       : List_Of_Tiles.List;
         Seen_Tiles           : List_Of_Tiles.List;
         Unseen_Tiles         : List_Of_Tiles.List;
         Target_Tiles         : List_Of_Tiles.List;
         Hostile_Tiles        : List_Of_Tiles.List;
         Active_Targets       : List_Of_Tiles.List;
         Tile_Info            : Tile_Info_Array;
         Hostile_Stacks       : Stack_Lists.List;
         Spotted_Hostiles     : Identifier_Sets.Map;
         Palace               : Carthage.Cities.City_Type;
         Shield               : Carthage.Cities.City_Type;
      end record;

   type Planet_Manager_Type is access all Planet_Manager_Record'Class;

   overriding procedure On_Hostile_Spotted
     (Manager : in out Planet_Manager_Record;
      Spotter : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class)
   is null;

   overriding function Average_Update_Frequency
     (Manager : Planet_Manager_Record)
      return Duration
   is (Carthage.Calendar.Days (1));

   overriding procedure Initialize
     (Manager : in out Planet_Manager_Record);

   overriding function Update
     (Manager : not null access Planet_Manager_Record)
      return Duration;

   procedure Scan_Unexplored_Tiles
     (Manager : in out Planet_Manager_Record'Class);

   function Create_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type;
      Active : Boolean)
      return Manager_Type;

   function Create_Active_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Manager_Type
   is (Create_Planet_Manager (House, Planet, True));

   function Create_Passive_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Manager_Type
   is (Create_Planet_Manager (House, Planet, False));

   ---------------------------
   -- Create_Planet_Manager --
   ---------------------------

   function Create_Planet_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type;
      Active : Boolean)
      return Manager_Type
   is

      Manager : constant Planet_Manager_Type :=
                  new Planet_Manager_Record;

      Group : constant Carthage.Managers.Cities.City_Trade_Group :=
                Carthage.Managers.Cities.New_Trade_Group;

      procedure Add_City_Manager
        (City : not null access constant Carthage.Cities.City_Record'Class);

      ----------------------
      -- Add_City_Manager --
      ----------------------

      procedure Add_City_Manager
        (City : not null access constant Carthage.Cities.City_Record'Class)
      is
         use type Carthage.Houses.House_Type;
      begin
         if City.Owner = House then
            Manager.City_Managers.Insert
              (City.Identifier,
               Carthage.Managers.Cities.Create_City_Manager
                 (House, Group, City));
         end if;
      end Add_City_Manager;

   begin
      Manager.House := House;
      Manager.Planet := Planet;
      Manager.Active := Active;

      Planet.Scan_Cities (Add_City_Manager'Access);

      Manager.Ground_Asset_Manager :=
        Carthage.Managers.Assets.Ground_Asset_Manager
          (Manager, House, Planet);

      Manager.Initialize;
      Add_Manager (Manager);

      return Manager_Type (Manager);
   end Create_Planet_Manager;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : in out Planet_Manager_Record)
   is
      use type Carthage.Houses.House_Type;

      procedure Classify_Tiles;

      procedure Scan_Area
        (Start : Tile_Position);

      procedure Classify_Tiles is
      begin
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
                          + Natural (Tile.Find_Stack
                                     (At_War_With'Access).Count)
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
               Palace_Tile     : constant Carthage.Tiles.Tile_Type :=
                                   Manager.Palace.Tile;
               Palace_Position : constant Tile_Position :=
                                   Palace_Tile.Position;
               Home_Continent  : constant Planet_Area_Lists.Cursor :=
                                   Manager.Tile_Info
                                     (Palace_Position.X, Palace_Position.Y)
                                     .Continent;
            begin
               for Tile of Manager.Areas (Home_Continent).Internal_Border loop
                  Manager.Tile_Info
                    (Tile.Position.X, Tile.Position.Y).Interest :=
                    100 + WL.Random.Random_Number (1, 100);
               end loop;
            end;
         end if;

      end Classify_Tiles;

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
                when Sea       => Tile.Is_Water,
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

      if Manager.Active then
         Classify_Tiles;
         Manager.Scan_Unexplored_Tiles;
      end if;

   end Initialize;

   ---------------------------
   -- Scan_Unexplored_Tiles --
   ---------------------------

   procedure Scan_Unexplored_Tiles
     (Manager : in out Planet_Manager_Record'Class)
   is
      Tiles : List_Of_Tiles.List renames Manager.Active_Targets;

      function Higher_Interest
        (Left, Right : Carthage.Tiles.Tile_Type)
         return Boolean
      is (Manager.Tile_Info (Left.Position.X, Left.Position.Y).Interest
          > Manager.Tile_Info (Right.Position.X, Right.Position.Y).Interest);

      package Sorting is
        new List_Of_Tiles.Generic_Sorting
          (Higher_Interest);

   begin
      Manager.Planet.Log
        (Manager.House.Name & ": looking for unexplored tiles");

      Tiles.Clear;

      for Unseen of Manager.Unseen_Tiles loop
         Tiles.Append (Unseen);
      end loop;
      for Seen of Manager.Seen_Tiles loop
         Tiles.Append (Seen);
      end loop;

      Manager.Planet.Log
        (Manager.House.Name & ":" & Natural'Image (Natural (Tiles.Length))
         & " tiles found");

      Sorting.Sort (Tiles);

      while not Tiles.Is_Empty loop

         declare
            Tile     : constant Carthage.Tiles.Tile_Type :=
                         Tiles.First_Element;
            New_List : List_Of_Tiles.List;
         begin

            Manager.House.Log
              ("interest:"
               & Manager.Tile_Info
                 (Tile.Position.X, Tile.Position.Y).Interest'Img
               & ": " & Manager.Planet.Tile (Tile.Position).Description);
            declare
               Goal : constant Carthage.Goals.Goal_Record'Class :=
                        Carthage.Managers.Assets.Tile_Reconnaissance_Goal
                          (Tile);
            begin
               if Manager.Ground_Asset_Manager
                 .Have_Immediate_Capacity (Goal)
               then
                  Manager.House.Log ("  adding goal: " & Goal.Show);
                  Manager.Ground_Asset_Manager.Add_Goal (Goal);
               else
                  exit;
               end if;
            end;

            for Check_Tile of Tiles loop
               if Carthage.Planets.Hex_Distance
                 (Tile.Position, Check_Tile.Position)
                 > 6
               then
                  New_List.Append (Check_Tile);
               end if;
            end loop;

            Tiles := New_List;
         end;

      end loop;

   end Scan_Unexplored_Tiles;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Manager : not null access Planet_Manager_Record)
      return Duration
   is
      use type Carthage.Cities.City_Type;
   begin
      Manager.Planet.Log ("updating for House " & Manager.House.Name);
      Manager.Check_Goals;

      if Manager.Palace /= null then
         declare
            Minimum, Desired : Carthage.Resources.Stock_Record;
         begin
            Manager.Ground_Asset_Manager.Get_Resource_Requirements
              (Minimum, Desired);
            Manager.Planet.Log ("ground asset desired resources");

            declare
               procedure Report_Stock
                 (Resource : Carthage.Resources.Resource_Type);

               ------------------
               -- Report_Stock --
               ------------------

               procedure Report_Stock
                 (Resource : Carthage.Resources.Resource_Type)
               is
                  Quantity : constant Natural :=
                               Desired.Whole_Quantity (Resource);
               begin
                  if Quantity > 0 then
                     Manager.Planet.Log
                       (Resource.Name & ":" & Quantity'Img);
                  end if;
               end Report_Stock;

            begin
               Carthage.Resources.Scan (Report_Stock'Access);
            end;

            Manager.Ground_Asset_Manager.Transfer_Resources
              (Manager.Palace.Update, Desired);
         end;
      end if;

      return Manager.Average_Update_Frequency;
   end Update;

end Carthage.Managers.Planets;
