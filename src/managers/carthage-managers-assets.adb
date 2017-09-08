package body Carthage.Managers.Assets is

   ----------------------
   -- Add_Capture_Goal --
   ----------------------

--     procedure Add_Capture_Goal
--       (Manager : in out Asset_Manager_Record;
--        Tile    : Carthage.Tiles.Tile_Type)
--     is
--     begin
--        Manager.Goals.Append
--          (Asset_Manager_Goal'
--             (Carthage.Goals.Goal_Record with
--                  Priority   => Default_Priority (Capture),
--                  Class      => Capture,
--                  Tile       => Tile,
--              Parameters =>
--                (Military => High, Spot => High, others => <>)));
--     end Add_Capture_Goal;

   -----------------
   -- Check_Goals --
   -----------------

   overriding function Check_Goal
     (Manager : Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Manager);
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);
   begin
      case Asset_Goal.Class is
         when None =>
            return True;
         when Recon =>
--              Manager.House.Log ("pretending we just checked "
--                                 & Asset_Goal.Tile.Description);
            return True;
         when Capture =>
--              Manager.House.Log ("pretending we just captured "
--                                 & Asset_Goal.Tile.Description);
            return True;
      end case;
   end Check_Goal;

   --------------------------
   -- Create_Asset_Manager --
   --------------------------

   function Create_Asset_Manager
     (House  : Carthage.Houses.House_Type;
      Planet : Carthage.Planets.Planet_Type)
      return Asset_Manager_Type
   is
   begin
      return new Asset_Manager_Record'
        (House  => House,
         Planet => Planet,
         others => <>);
   end Create_Asset_Manager;

   ------------------------
   -- Load_Initial_State --
   ------------------------

   overriding procedure Load_Initial_State
     (Manager : in out Asset_Manager_Record)
   is
      procedure Add_Stack
        (Stack : not null access constant
           Carthage.Stacks.Stack_Record'Class);

      ---------------
      -- Add_Stack --
      ---------------

      procedure Add_Stack
        (Stack : not null access constant
           Carthage.Stacks.Stack_Record'Class)
      is
      begin
         Manager.Stacks.Append
           (Managed_Stack_Record'
              (Stack => Carthage.Stacks.Stack_Type (Stack),
               Goal  => <>));
         for I in 1 .. Stack.Count loop
            Manager.Assets.Append
              (Managed_Asset_Record'
                 (Asset => Stack.Asset (I),
                  Stack => Carthage.Stacks.Stack_Type (Stack),
                  Tile  => Stack.Tile));
         end loop;
      end Add_Stack;

   begin
      Manager.Planet.Scan_Stacks
        (Manager.House,
         Add_Stack'Access);
   end Load_Initial_State;

   ----------------
   -- Recon_Goal --
   ----------------

   function Recon_Goal
     (Manager : Asset_Manager_Record;
      Tile    : Carthage.Tiles.Tile_Type)
      return Carthage.Goals.Goal_Record'Class
   is
      pragma Unreferenced (Manager);
   begin
      return Result : Asset_Manager_Goal (Default_Priority (Recon)) do
         Result.Class := Recon;
         Result.Tile := Tile;
         Result.Parameters :=
           (Speed => High, Spot => High, others => <>);
      end return;
   end Recon_Goal;

end Carthage.Managers.Assets;
