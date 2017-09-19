with Ada.Calendar;
with Ada.Text_IO;

with Carthage.Calendar;

with Carthage.Cities.Updates;
with Carthage.Houses;
with Carthage.Planets;
with Carthage.Stacks.Updates;
with Carthage.Structures;
with Carthage.Tiles;
with Carthage.Units;

with Carthage.Combat;
with Carthage.Managers;

with Carthage.Logging;

package body Carthage.Updates is

   task Update_Task is
      entry Start;
      entry Stop;
      entry Set_Speed (Speed : Update_Speed);
      entry Render_Started;
      entry Render_Finished;
   end Update_Task;

   -------------------------
   -- Before_First_Update --
   -------------------------

   procedure Before_First_Update is

      procedure Reset_Planet_State
        (Planet : Carthage.Planets.Planet_Type);

      procedure Add_Planet_Maps (House : Carthage.Houses.House_Type);

      procedure City_Look
        (City : Carthage.Cities.City_Type);

      procedure Find_Agora
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
            declare
               Tile : constant Carthage.Tiles.Tile_Type :=
                        Carthage.Planets.Get_Tile (Tiles, I);
            begin
               Tile.Update.Set_Currently_Visible_To (City.Owner);
            end;
         end loop;

      end City_Look;

      procedure Find_Agora
        (City : Carthage.Cities.City_Type)
      is
         use Carthage.Cities;
         use type Carthage.Houses.Treaty_Status;

         Minimum_Distance : Natural := Natural'Last;
         Closest_Agora    : City_Type := null;

         procedure Check (Check_City : not null access constant City_Class);

         -----------
         -- Check --
         -----------

         procedure Check (Check_City : not null access constant City_Class) is
            D : constant Natural :=
                  Carthage.Planets.Hex_Distance
                    (Check_City.Tile.Position, City.Tile.Position);
         begin
            if Check_City.Is_Agora
              and then City.Owner.Treaty_Status_With (Check_City.Owner)
              /= Carthage.Houses.War
              and then D < Minimum_Distance
            then
               Closest_Agora := City_Type (Check_City);
               Minimum_Distance := D;
            end if;
         end Check;

      begin

         if not City.Is_Agora then

            City.Planet.Scan_Cities (Check'Access);

            if Closest_Agora /= null then
               City.Update.Set_Agora (Closest_Agora);
            end if;
         end if;

      end Find_Agora;

      ------------------------
      -- Reset_Planet_State --
      ------------------------

      procedure Reset_Planet_State
        (Planet : Carthage.Planets.Planet_Type)
      is
      begin
         Planet.Update.Clear_Visibility;
      end Reset_Planet_State;

      -------------------
      -- Reveal_Planet --
      -------------------

      procedure Reveal_Planet
        (Planet   : Carthage.Planets.Planet_Type;
         House    : Carthage.Houses.House_Type;
         Explored : Boolean)
      is
         Tiles : Carthage.Planets.Surface_Tiles;
      begin
         if Explored then
            House.Log (Planet.Name & ": fully explored");
            Planet.Update.Set_Explored_By (House);
         else
            House.Log (Planet.Name & ": map revealed");
            Planet.Update.Set_Seen_By (House);
         end if;

         Planet.Get_Tiles (Tiles);
         for I in 1 .. Carthage.Planets.Tile_Count (Tiles) loop
            declare
               Tile : constant Carthage.Tiles.Tile_Type :=
                        Carthage.Planets.Get_Tile (Tiles, I);
            begin
               if Explored then
                  Tile.Update.Set_Explored_By (House);
               else
                  Tile.Update.Set_Seen_By (House);
               end if;
            end;
         end loop;
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
               declare
                  Tile : constant Carthage.Tiles.Tile_Type :=
                           Carthage.Planets.Get_Tile (Tiles, I);
               begin
                  Tile.Update.Set_Currently_Visible_To (Stack.Owner);
               end;
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

      Carthage.Cities.Scan_Cities
        (Find_Agora'Access);

      Carthage.Stacks.Scan_Stacks
        (Stack_Look'Access);

      Carthage.Managers.Start_Managers;

   end Before_First_Update;

   ---------------------
   -- Render_Finished --
   ---------------------

   procedure Render_Finished is
   begin
      Update_Task.Render_Finished;
   end Render_Finished;

   --------------------
   -- Render_Started --
   --------------------

   procedure Render_Started is
   begin
      Update_Task.Render_Started;
   end Render_Started;

   ---------------
   -- Set_Speed --
   ---------------

   procedure Set_Speed (Speed : Update_Speed) is
   begin
      Update_Task.Set_Speed (Speed);
   end Set_Speed;

   -------------------
   -- Start_Updates --
   -------------------

   procedure Start_Updates is
   begin
      Update_Task.Start;
   end Start_Updates;

   ------------------
   -- Stop_Updates --
   ------------------

   procedure Stop_Updates is
   begin
      Update_Task.Stop;
   end Stop_Updates;

   ------------
   -- Update --
   ------------

   procedure Update is

      procedure Execute_Round
        (Battle : in out Carthage.Combat.Battle_Record);

      -------------------
      -- Execute_Round --
      -------------------

      procedure Execute_Round
        (Battle : in out Carthage.Combat.Battle_Record)
      is
         use Carthage.Combat;
      begin
         Attacker (Battle).Log ("attacking " & Defender (Battle).Name);
         for Weapon in Carthage.Units.Weapon_Category loop
            declare
               Round : constant Carthage.Combat.Attack_Record_Array :=
                         Carthage.Combat.Attack_Round (Battle, Weapon);
            begin
               for Attack of Round loop
                  Attacker (Battle).Log (Image (Attack));
               end loop;
            end;
         end loop;
      end Execute_Round;

   begin
      Ada.Text_IO.Put_Line
        ("Update: "
         & Carthage.Calendar.Day_Identifier (Carthage.Calendar.Today));
      Carthage.Logging.Log ("starting update");
      Carthage.Managers.Start_Manager_Turns;
      Carthage.Managers.Execute_Manager_Turns;
      Carthage.Cities.Updates.Execute_Orders;
      Carthage.Cities.Updates.Execute_Production;
      Carthage.Stacks.Updates.Execute_Orders;
      Carthage.Houses.Scan (Carthage.Houses.Log_Status'Access);
      Carthage.Combat.Scan_Battles (Execute_Round'Access);
      Carthage.Stacks.Remove_Empty_Ground_Stacks;
      Carthage.Logging.Log ("update complete");
      Carthage.Calendar.Next_Day;
   end Update;

   -----------------
   -- Update_Task --
   -----------------

   task body Update_Task is
      Current_Speed : Update_Speed := 0;
      Update_Delay  : constant array (Update_Speed) of Duration :=
                        (0 => 100.0,
                         1 => 1.0,
                         2 => 0.5,
                         3 => 0.1);
      Next_Update   : Ada.Calendar.Time;

      procedure Schedule_Next_Update;

      --------------------------
      -- Schedule_Next_Update --
      --------------------------

      procedure Schedule_Next_Update is
         use Ada.Calendar;
      begin
         Next_Update := Clock + Update_Delay (Current_Speed);
      end Schedule_Next_Update;

   begin
      accept Start;
      Schedule_Next_Update;

      loop
         select
            accept Stop;
            exit;
         or
            accept Set_Speed (Speed : in Update_Speed) do
               Current_Speed := Speed;
            end Set_Speed;
            Schedule_Next_Update;
         or
            accept Render_Started;
            accept Render_Finished;
         or
            delay until Next_Update;
            if Current_Speed > 0 then
               Update;
               Schedule_Next_Update;
            end if;
         end select;
      end loop;
   end Update_Task;

end Carthage.Updates;
