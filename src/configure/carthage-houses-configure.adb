with Ada.Text_IO;

with Carthage.Structures;
with Carthage.Planets;
with Carthage.Tiles;
with Carthage.Units;

with Carthage.Cities.Create;
with Carthage.Assets.Create;
with Carthage.Stacks.Create;

with Carthage.Import;

package body Carthage.Houses.Configure is

   Current_House_Flag : House_Set := 0;

   function Is_Land_Tile
     (Tile : Carthage.Tiles.Tile_Type)
      return Boolean
   is (not Tile.Is_Water);

   function Is_City_Tile
     (Tile : Carthage.Tiles.Tile_Type)
      return Boolean
   is (Tile.Has_City);

   ---------------------
   -- Configure_House --
   ---------------------

   procedure Configure_House
     (Config : Tropos.Configuration)
   is
      procedure Create (House : in out House_Class);

      ------------
      -- Create --
      ------------

      procedure Create (House : in out House_Class) is
      begin
         House.Create_With_Identity
           (Config.Config_Name);

         if Current_House_Flag = 0 then
            Current_House_Flag := 1;
         else
            Current_House_Flag := Current_House_Flag * 2;
         end if;

         House.Set_Flag := Current_House_Flag;

         House.Category :=
           House_Category'Value
             (Config.Get ("category"));
         House.Colour :=
           (if Config.Contains ("colour")
            then Carthage.Import.Palette_Colour
              (Config.Get ("colour"))
            else (0.5, 0.5, 0.5, 1.0));

         declare
            Capital : constant String :=
                        Config.Get ("capital", "");
         begin
            if Capital /= "" then
               if Carthage.Planets.Exists (Capital) then
                  House.Capital := Carthage.Planets.Get (Capital);

                  declare
                     procedure Set_Owner
                       (Planet : in out Carthage.Planets.Planet_Class);

                     ---------------
                     -- Set_Owner --
                     ---------------

                     procedure Set_Owner
                       (Planet : in out Carthage.Planets.Planet_Class)
                     is
                     begin
                        Planet.Set_Owner (Db.Reference (House));
                     end Set_Owner;

                  begin
                     Carthage.Planets.Update
                       (House.Capital, Set_Owner'Access);
                  end;

               end if;
            end if;
         end;

         if Config.Contains ("reveal") then
            for Revealed_Planet of Config.Child ("reveal") loop
               House.Known_Planets.Append (Revealed_Planet.Config_Name);
            end loop;
         end if;

         House.Log ("created " & House.Full_Name);
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_House;

   ---------------------
   -- Configure_Start --
   ---------------------

   procedure Configure_Start
     (House        : House_Type;
      Start_Config : Tropos.Configuration)
   is
      use Carthage.Planets;

      Planet : constant Planet_Type := Planet_Type (House.Capital);
      Shield_Position : constant Tile_Position := (1, 1);
      Available       : Carthage.Planets.Surface_Tiles;

      Minimum_Harvester_Distance : constant Natural :=
                                     Start_Config.Get
                                       ("minimum-harvester-start-distance",
                                        4)
        with Unreferenced;
      Maximum_Harvester_Distance : constant Natural :=
                                     Start_Config.Get
                                       ("maximum-harvester-start-distance",
                                        15);

      procedure Configure_City_Group
        (Middle_Structure  : String;
         Inner_Structures : Tropos.Configuration);

      procedure Configure_Harvester_City
        (Structure : Carthage.Structures.Structure_Type)
        with Pre => Structure.Is_Harvester;

      function Best_Available_Harvester_Tile
        (Structure : Carthage.Structures.Structure_Type)
         return Carthage.Tiles.Tile_Type;

      -----------------------------------
      -- Best_Available_Harvester_Tile --
      -----------------------------------

      function Best_Available_Harvester_Tile
        (Structure : Carthage.Structures.Structure_Type)
         return Carthage.Tiles.Tile_Type
      is
         Best_Index : Natural := 0;
         Best_Score : Integer := Integer'First;

      begin
         for I in 1 .. Tile_Count (Available) loop
            declare
               Tile       : constant Carthage.Tiles.Tile_Type :=
                              Get_Tile (Available, I);
               This_Score : Integer := 0;
               OK         : Boolean := not Tile.Has_City;
            begin
               if OK then
                  declare
                     Local_Tiles : Surface_Tiles;
                  begin
                     Planet.Get_Tiles
                       (Origin       => Tile,
                        Min_Distance => 0,
                        Max_Distance => Structure.Radius + 1,
                        Test         => null,
                        Tiles        => Local_Tiles);

                     for J in 1 .. Tile_Count (Local_Tiles) loop
                        declare
                           Local_Tile : constant Carthage.Tiles.Tile_Type :=
                                          Get_Tile (Local_Tiles, J);
                        begin

                           if Local_Tile.Has_City
                             and then not Local_Tile.City.Structure.Is_Bonus
                           then
                              OK := False;
                              exit;
                           end if;

                           for Item of
                             Structure.Harvest_Production (Local_Tile)
                           loop
                              This_Score := This_Score
                                + Natural (Item.Quantity);
                           end loop;

                           if Local_Tile.Has_City
                             and then Local_Tile.City.Structure.Is_Bonus
                           then
                              This_Score := This_Score
                                + Structure.Bonus_Production
                                (Local_Tile.City.Structure);
                           end if;
                        end;
                     end loop;
                  end;
               end if;

               if OK then
                  This_Score := This_Score -
                    Carthage.Planets.Hex_Distance
                      (Shield_Position, Tile.Position)
                    / 5;
               end if;

               if OK then
                  if This_Score > Best_Score then
                     Best_Score := This_Score;
                     Best_Index := I;
                  end if;
               end if;
            end;
         end loop;

         if Best_Index = 0 then
            raise Constraint_Error with
              "could not find any tiles for " & Structure.Identifier;
         end if;

         return Get_Tile (Available, Best_Index);

      end Best_Available_Harvester_Tile;

      --------------------------
      -- Configure_City_Group --
      --------------------------

      procedure Configure_City_Group
        (Middle_Structure  : String;
         Inner_Structures : Tropos.Configuration)
      is
         Middle : Carthage.Structures.Structure_Type;
         Facs   : array (1 .. Inner_Structures.Child_Count) of
           Carthage.Structures.Structure_Type;

         function Position_OK
           (Position : Tile_Position)
            return Boolean
           with Unreferenced;

         -----------------
         -- Position_OK --
         -----------------

         function Position_OK
           (Position : Tile_Position)
            return Boolean
         is

         begin
            if not Middle.Terrain_OK
              (Planet.Tile (Position).Base_Terrain)
              or else Planet.Tile (Position).Has_City
            then
               return False;
            end if;

            declare
               Ns : constant Array_Of_Tiles :=
                      Planet.Neighbour_Tiles (Position);
               Used : array (Ns'Range) of Boolean := (others => False);
            begin
               for Neighbour_Tile_Index in Ns'Range loop
                  if Ns (Neighbour_Tile_Index).Has_City then
                     return False;
                  end if;

                  declare
                     OK : Boolean := False;
                  begin
                     for Fac of Facs loop
                        if not Used (Neighbour_Tile_Index)
                          and then Fac.Terrain_OK
                            (Ns (Neighbour_Tile_Index).Base_Terrain)
                        then
                           Used (Neighbour_Tile_Index) := True;
                           OK := True;
                           exit;
                        end if;
                     end loop;
                     if not OK then
                        return False;
                     end if;
                  end;
               end loop;
            end;

            declare
               Reachable : Surface_Tiles;
            begin
               Planet.Get_Tiles (Planet.Tile (Position),
                                 1, Maximum_Harvester_Distance,
                                 Is_Land_Tile'Access, Reachable);
               Carthage.Planets.Remove_Tiles
                 (Reachable, Is_City_Tile'Access);

               Ada.Text_IO.Put_Line
                 (Position.X'Img & Position.Y'Img & ": reachable tiles:"
                  & Natural'Image (Tile_Count (Reachable)));
               return Tile_Count (Reachable) > 80;
            end;

         end Position_OK;

      begin

         if not Carthage.Structures.Exists (Middle_Structure) then
            raise Constraint_Error with
              "while configuring " & House.Name & " start: "
              & Middle_Structure & ": no such city type";
         end if;

         Middle := Carthage.Structures.Get (Middle_Structure);

         for I in 1 .. Inner_Structures.Child_Count loop
            if not Carthage.Structures.Exists (Inner_Structures.Get (I)) then
               raise Constraint_Error with
                 "while configuring " & House.Name & " start: "
                 & Inner_Structures.Get (I) & ": no such city type";
            end if;

            Facs (I) :=
              Carthage.Structures.Get
                (String'(Inner_Structures.Get (I)));
         end loop;

--           declare
--              Middle_Position : constant Tile_Position :=
--                                  Planet.Find_Tile
--                                    ((Planet_Width / 2, Planet_Height / 2),
--                                     Position_OK'Access);
--           begin
--
--              Shield_Position := Middle_Position;
--
--              Carthage.Cities.Create.New_City
--                (Planet   => Planet,
--                 Tile     => House.Capital.Tile (Middle_Position),
--                 Structure => Middle,
--                 Owner     => House);
--
--              Planet.Tile (Middle_Position).Update.Set_Road (True);
--
--              declare
--                 Ns   : constant Array_Of_Tiles :=
--                          Planet.Neighbour_Tiles (Middle_Position);
--                 Used : array (Ns'Range) of Boolean := (others => False);
--              begin
--                 for Fac of Facs loop
--                    for Tile_Index in Ns'Range loop
--                       if not Used (Tile_Index)
--                         and then Fac.Terrain_OK
--                           (Ns (Tile_Index).Base_Terrain)
--                       then
--                          Carthage.Cities.Create.New_City
--                            (Planet   => Planet,
--                             Tile     => Ns (Tile_Index),
--                             Structure => Fac,
--                             Owner     => House);
--                          Ns (Tile_Index).Update.Set_Road (True);
--                          Used (Tile_Index) := True;
--                          exit;
--                       end if;
--                    end loop;
--                 end loop;
--              end;
--
--           end;
      end Configure_City_Group;

      ------------------------------
      -- Configure_Harvester_City --
      ------------------------------

      procedure Configure_Harvester_City
        (Structure : Carthage.Structures.Structure_Type)
      is
         Tile  : constant Carthage.Tiles.Tile_Type :=
                   Best_Available_Harvester_Tile (Structure);

         function Passable
           (Tile : Carthage.Tiles.Tile_Type)
            return Boolean
         is (not Tile.Is_Water);

         function Road_Cost
           (Tile : Carthage.Tiles.Tile_Type)
            return Float
         is (if Tile.Has_Road
             then 0.0
             elsif Tile.Is_Water
             then Float'Last
             else Float (Planet.Road_Cost (Tile.Position)));

         Road : constant Array_Of_Positions :=
                  Planet.Find_Path
                    (Shield_Position, Tile.Position,
                     Passable'Access, Road_Cost'Access);

      begin
         Carthage.Cities.Create.New_City
           (Planet    => Planet,
            Tile      => Tile,
            Structure => Structure,
            Owner     => House,
            Health    => 75,
            Loyalty   => 75);

         for Position of Road loop
            Planet.Tile (Position).Update.Set_Road (True);
         end loop;

         Remove_Tiles (Available, Tile.Position, 5);

      end Configure_Harvester_City;

   begin

      Ada.Text_IO.Put_Line ("Configuring start for " & House.Full_Name);

      for Config of Start_Config.Child ("cities") loop
         if Config.Config_Name = "group" then
            Configure_City_Group (Config.Child ("middle").Value,
                                  Config.Child ("cities"));

            Ada.Text_IO.Put_Line
              ("   shield placed on " & Planet.Name
               & " at" & Shield_Position.X'Img
               & Shield_Position.Y'Img);

            Planet.Get_Tiles (Planet.Tile (Shield_Position),
                              1, Maximum_Harvester_Distance,
                              Is_Land_Tile'Access, Available);
            Carthage.Planets.Remove_Tiles
              (Available, Is_City_Tile'Access);

            Ada.Text_IO.Put_Line
              ("available tiles:"
               & Natural'Image (Tile_Count (Available)));
         elsif Carthage.Structures.Exists (Config.Config_Name) then
            for I in 1 .. Config.Value loop
               Ada.Text_IO.Put_Line
                 ("city: " & Config.Config_Name
                  & "; available tiles:"
                  & Natural'Image (Tile_Count (Available)));
               declare
                  Structure : constant Carthage.Structures.Structure_Type :=
                                Carthage.Structures.Get (Config.Config_Name);
               begin
                  Configure_Harvester_City (Structure);
               end;
            end loop;
         end if;
      end loop;

      for Stack_Config of Start_Config.Child ("stacks") loop
         declare
            function Tile_OK (Position : Tile_Position) return Boolean
            is (not Planet.Tile (Position).Is_Water
                and then not Planet.Tile (Position).Has_Stacks)
            with Unreferenced;

            Start      : constant Tile_Position := (1, 1);
--                             Planet.Find_Tile
--                               (Shield_Position, Tile_OK'Access);
            Stack      : constant Carthage.Stacks.Stack_Type :=
                           Carthage.Stacks.Create.New_Ground_Stack
                             (House, Planet, Planet.Tile (Start));
         begin
            for Asset_Config of Stack_Config loop
               declare
                  Unit_Name  : constant String :=
                                 Asset_Config.Get ("unit");
                  Unit_Count : constant Natural :=
                                 Asset_Config.Get ("count", 1);
                  Unit       : constant Carthage.Units.Unit_Type :=
                                 Carthage.Units.Get (Unit_Name);
               begin
                  for I in 1 .. Unit_Count loop
                     declare
                        Asset : constant Carthage.Assets.Asset_Type :=
                                  Carthage.Assets.Create.New_Asset
                                    (Unit    => Unit,
                                     Owner   => House,
                                     XP      => Carthage.Assets.Expert);
                     begin
                        Asset.Move_To (Stack);
                     end;
                  end loop;
               end;
            end loop;
         end;
      end loop;

   end Configure_Start;

end Carthage.Houses.Configure;
