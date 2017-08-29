with Carthage.Cities;
with Carthage.Planets;
with Carthage.Stacks;
with Carthage.Structures;
with Carthage.Tiles;

package body Carthage.Updates is

   -----------------
   -- Before_Turn --
   -----------------

   procedure Before_Turn is

      procedure Reset_Planet_State
        (Planet : Carthage.Planets.Planet_Type);

      procedure City_Look
        (City : Carthage.Cities.City_Type);

      procedure Stack_Look
        (Stack : Carthage.Stacks.Stack_Type);

      procedure Set_Planet_Owner
        (Palace : Carthage.Cities.City_Type);

      ---------------
      -- City_Look --
      ---------------

      procedure City_Look
        (City : Carthage.Cities.City_Type)
      is
         Tiles : Carthage.Planets.Surface_Tiles;
      begin
         City.Planet.Get_Tiles
           (Origin       => City.Tile,
            Min_Distance => 0,
            Max_Distance => 6,
            Test         => null,
            Tiles        => Tiles);

         for I in 1 .. Carthage.Planets.Tile_Count (Tiles) loop
            Carthage.Tiles.Set_Currently_Visible_To
              (Tile  => Carthage.Planets.Get_Tile (Tiles, I),
               House => City.Owner);
         end loop;

      end City_Look;

      ------------------------
      -- Reset_Planet_State --
      ------------------------

      procedure Reset_Planet_State
        (Planet : Carthage.Planets.Planet_Type)
      is
      begin
         Carthage.Planets.Update
           (Planet, Carthage.Planets.Clear_Visibility'Access);
      end Reset_Planet_State;

      ----------------------
      -- Set_Planet_Owner --
      ----------------------

      procedure Set_Planet_Owner
        (Palace : Carthage.Cities.City_Type)
      is
         procedure Update (Planet : in out Carthage.Planets.Planet_Class);

         ------------
         -- Update --
         ------------

         procedure Update (Planet : in out Carthage.Planets.Planet_Class) is
         begin
            Planet.Set_Owner (Palace.Owner);
         end Update;

      begin
         Carthage.Planets.Update (Palace.Planet, Update'Access);
      end Set_Planet_Owner;

      ----------------
      -- Stack_Look --
      ----------------

      procedure Stack_Look
        (Stack : Carthage.Stacks.Stack_Type)
      is
         Tiles : Carthage.Planets.Surface_Tiles;
      begin

         if Stack.Has_Tile then
            Stack.Planet.Get_Tiles
              (Origin       => Stack.Tile,
               Min_Distance => 0,
               Max_Distance => 6,
               Test         => null,
               Tiles        => Tiles);

            for I in 1 .. Carthage.Planets.Tile_Count (Tiles) loop
               Carthage.Tiles.Set_Currently_Visible_To
                 (Tile  => Carthage.Planets.Get_Tile (Tiles, I),
                  House => Stack.Owner);
            end loop;
         elsif Stack.In_Space then
            Stack.Planet.Get_Tiles (Tiles);
            for I in 1 .. Carthage.Planets.Tile_Count (Tiles) loop
               Carthage.Tiles.Set_Seen_By
                 (Tile  => Carthage.Planets.Get_Tile (Tiles, I),
                  House => Stack.Owner);
            end loop;
         end if;
      end Stack_Look;

   begin

      Carthage.Planets.Scan (Reset_Planet_State'Access);

      Carthage.Cities.Scan_Cities
        (Carthage.Structures.Get ("palace"),
         Set_Planet_Owner'Access);

      Carthage.Cities.Scan_Cities
        (City_Look'Access);

      Carthage.Stacks.Scan_Stacks
        (Stack_Look'Access);

   end Before_Turn;

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      null;
   end Update;

end Carthage.Updates;
