with Carthage.Cities.Updates;
with Carthage.Houses;
with Carthage.Managers;
with Carthage.Planets;
with Carthage.Stacks;
with Carthage.Structures;
with Carthage.Tiles;

package body Carthage.Updates is

   -----------------
   -- Before_Turn --
   -----------------

   procedure Before_First_Turn is

      procedure Reset_Planet_State
        (Planet : Carthage.Planets.Planet_Type);

      procedure Add_Planet_Maps (House : Carthage.Houses.House_Type);

      procedure City_Look
        (City : Carthage.Cities.City_Type);

      procedure Stack_Look
        (Stack : Carthage.Stacks.Stack_Type);

      procedure Set_Planet_Owner
        (Palace : Carthage.Cities.City_Type);

      procedure Reveal_Planet
        (Planet : Carthage.Planets.Planet_Type;
         House    : Carthage.Houses.House_Type;
         Explored : Boolean);

      ---------------------
      -- Add_Planet_Maps --
      ---------------------

      procedure Add_Planet_Maps (House : Carthage.Houses.House_Type) is

         procedure Add_Planet (Id : String);

         ----------------
         -- Add_Planet --
         ----------------

         procedure Add_Planet (Id : String) is
            Planet : constant Carthage.Planets.Planet_Type :=
                       (if Carthage.Planets.Exists (Id)
                        then Carthage.Planets.Get (Id)
                        else raise Constraint_Error with
                          "no such planet: " & Id);
         begin
            Reveal_Planet (Planet, House, True);
         end Add_Planet;

      begin
         House.Scan_Known_Planets (Add_Planet'Access);
      end Add_Planet_Maps;

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

      -------------------
      -- Reveal_Planet --
      -------------------

      procedure Reveal_Planet
        (Planet : Carthage.Planets.Planet_Type;
         House    : Carthage.Houses.House_Type;
         Explored : Boolean)
      is
         Tiles : Carthage.Planets.Surface_Tiles;
      begin
         if Explored then
            House.Log (Planet.Name & ": fully explored");
         else
            House.Log (Planet.Name & ": map revealed");
         end if;

         Planet.Get_Tiles (Tiles);
         for I in 1 .. Carthage.Planets.Tile_Count (Tiles) loop
            Carthage.Tiles.Set_Seen_By
              (Tile  => Carthage.Planets.Get_Tile (Tiles, I),
               House => House);
            if Explored then
               Carthage.Tiles.Set_Explored_By
                 (Tile  => Carthage.Planets.Get_Tile (Tiles, I),
                  House => House);
            end if;
         end loop;
         Carthage.Planets.Set_Seen_By (Planet, House);
      end Reveal_Planet;

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
         use type Carthage.Stacks.Asset_Count;
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
         elsif Stack.In_Space
           and then Stack.Count > 0
             and then not Stack.Planet.Seen_By (Stack.Owner)
         then
            Reveal_Planet (Stack.Planet, Stack.Owner, False);
         end if;
      end Stack_Look;

   begin

      Carthage.Planets.Scan (Reset_Planet_State'Access);

      Carthage.Houses.Scan (Add_Planet_Maps'Access);

      Carthage.Cities.Scan_Cities
        (Carthage.Structures.Get ("palace"),
         Set_Planet_Owner'Access);

      Carthage.Cities.Scan_Cities
        (City_Look'Access);

      Carthage.Stacks.Scan_Stacks
        (Stack_Look'Access);

   end Before_First_Turn;

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Carthage.Managers.Before_Start_Of_Turn;
      Carthage.Managers.Create_Orders;
      Carthage.Cities.Updates.Execute_Production;
   end Update;

end Carthage.Updates;
