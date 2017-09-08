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

   -------------------------------
   -- Get_Resource_Requirements --
   -------------------------------

   procedure Get_Resource_Requirements
     (Manager : in out Asset_Manager_Record;
      Minimum : in out Carthage.Resources.Stock_Interface'Class;
      Desired : in out Carthage.Resources.Stock_Interface'Class)
   is
      Food_Resource : constant Carthage.Resources.Resource_Type :=
                        Carthage.Resources.Food;
      Min_Food     : Natural := 0;
      Desired_Food : Natural := 0;
   begin
      for Stack of Manager.Stacks loop
         declare
            Have_Food : Natural := 0;
            Eat_Food  : Natural := 0;
         begin
            for I in 1 .. Stack.Stack.Count loop
               declare
                  Asset : constant Carthage.Assets.Asset_Type :=
                            Stack.Stack.Asset (I);
               begin
                  Have_Food := Have_Food + Asset.Quantity (Food_Resource);
                  Eat_Food  := Eat_Food + Asset.Unit.Eat;
               end;
            end loop;

            Stack.Minimum_Food := 0;
            if Eat_Food > Have_Food then
               Stack.Minimum_Food := Eat_Food - Have_Food;
               Min_Food := Min_Food + Stack.Minimum_Food;
            end if;
            Stack.Desired_Food := Eat_Food;
            Desired_Food := Desired_Food + Eat_Food;
         end;
      end loop;
      Manager.House.Log
        (Manager.Planet.Identifier & ": minimum food =" & Min_Food'Img
         & "; desired food =" & Desired_Food'Img);
      Manager.Minimum_Food := Min_Food;
      Manager.Desired_Food := Desired_Food;
      Minimum.Add (Food_Resource, Min_Food);
      Desired.Add (Food_Resource, Desired_Food);
   end Get_Resource_Requirements;

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
               Goal  => <>,
               Minimum_Food => 0,
               Desired_Food => 0));

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

   procedure Transfer_Resources
     (Manager   : in out Asset_Manager_Record;
      Resources : in out Carthage.Resources.Stock_Interface'Class)
   is null;

end Carthage.Managers.Assets;
