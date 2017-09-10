with Carthage.Stacks.Create;

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

   --------------
   -- Add_Goal --
   --------------

   overriding procedure Add_Goal
     (Manager : not null access Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
   is
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);

      function Best_Available
        (List   : Asset_Classification_List.List;
         Target : Tile_Position)
         return Managed_Asset_List.Cursor;

      --------------------
      -- Best_Available --
      --------------------

      function Best_Available
        (List   : Asset_Classification_List.List;
         Target : Tile_Position)
         return Managed_Asset_List.Cursor
      is
         Best_Asset    : Managed_Asset_List.Cursor :=
                           Managed_Asset_List.No_Element;
         Best_Distance : Natural := Natural'Last;

      begin
         for Cursor of List loop
            declare
               Managed_Asset : Managed_Asset_Record renames
                                 Manager.Assets (Cursor);
               Managed_Stack : Managed_Stack_Record renames
                                 Manager.Stacks (Managed_Asset.Stack);
            begin
               if not Goal_Lists.Has_Element (Managed_Stack.Goal) then
                  declare
                     Position : constant Tile_Position :=
                                  Manager.Stacks (Managed_Asset.Stack)
                                  .Stack.Tile.Position;
                     Distance : constant Natural :=
                                  Carthage.Planets.Hex_Distance
                                    (Position, Target);
                  begin
                     if not Managed_Asset_List.Has_Element (Best_Asset)
                       or else Distance < Best_Distance
                     then
                        Best_Asset := Cursor;
                        Best_Distance := Distance;
                     end if;
                  end;
               end if;
            end;
         end loop;
         return Best_Asset;
      end Best_Available;

   begin
      case Asset_Goal.Class is
         when None =>
            null;
         when Recon =>
            declare
               use type Carthage.Stacks.Asset_Count;
               Asset_Cursor : constant Managed_Asset_List.Cursor :=
                                Best_Available (Manager.Spotters,
                                                Asset_Goal.Tile.Position);
               Asset        : constant Carthage.Assets.Asset_Type :=
                                Manager.Assets (Asset_Cursor).Asset;
               Stack_Cursor : Managed_Stack_List.Cursor :=
                                Manager.Assets (Asset_Cursor).Stack;
               Stack        : Carthage.Stacks.Stack_Type :=
                                  Manager.Stacks (Stack_Cursor).Stack;
            begin
               if Stack.Count > 1 then
                  declare
                     New_Stack : constant Carthage.Stacks.Stack_Type :=
                                   Carthage.Stacks.Create.New_Ground_Stack
                                     (Stack.Owner, Stack.Planet, Stack.Tile);
                  begin
                     Stack.Update.Remove_Asset (Asset);
                     New_Stack.Update.Add_Asset (Asset);
                     New_Stack.Update.Set_Manager (Manager);
                     Manager.Stacks.Append
                       ((New_Stack, others => <>));
                     Stack_Cursor := Manager.Stacks.Last;
                     Manager.Assets (Asset_Cursor).Stack := Stack_Cursor;
                     Stack := New_Stack;
                  end;
               end if;

               Asset.Log ("assigned to recon of "
                          & Asset_Goal.Tile.Description);
               Manager.Goals.Append (Goal);
               Manager.Stacks (Stack_Cursor).Goal :=
                 Manager.Goals.Last;
               Manager.Stacks (Stack_Cursor).Stack.Update.Move_To_Tile
                 (Asset_Goal.Tile);
            end;

         when Capture =>
            null;
      end case;
   end Add_Goal;

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

   -----------------------------
   -- Have_Immediate_Capacity --
   -----------------------------

   overriding function Have_Immediate_Capacity
     (Manager : Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);

      function Have_Capacity
        (List : Asset_Classification_List.List)
         return Boolean;

      -------------------
      -- Have_Capacity --
      -------------------

      function Have_Capacity
        (List : Asset_Classification_List.List)
         return Boolean
      is
      begin
         for Position of List loop
            declare
               Managed_Asset : Managed_Asset_Record renames
                                 Manager.Assets (Position);
               Managed_Stack : Managed_Stack_Record renames
                                 Manager.Stacks (Managed_Asset.Stack);
            begin
               if not Goal_Lists.Has_Element (Managed_Stack.Goal) then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Have_Capacity;

   begin
      case Asset_Goal.Class is
         when None =>
            return True;
         when Recon =>
            return Have_Capacity (Manager.Spotters);
         when Capture =>
            return False;
      end case;
   end Have_Immediate_Capacity;

   ------------------------
   -- Load_Initial_State --
   ------------------------

   overriding procedure Load_Initial_State
     (Manager : not null access Asset_Manager_Record)
   is
      procedure Add_Stack
        (Stack : not null access constant
           Carthage.Stacks.Stack_Record'Class);

      procedure Add_Asset_Classification
        (Asset_Cursor : Managed_Asset_List.Cursor;
         To_List      : in out Asset_Classification_List.List;
         Rate         : not null access
           function (Asset : Carthage.Assets.Asset_Type) return Natural);

      ------------------------------
      -- Add_Asset_Classification --
      ------------------------------

      procedure Add_Asset_Classification
        (Asset_Cursor : Managed_Asset_List.Cursor;
         To_List      : in out Asset_Classification_List.List;
         Rate         : not null access
           function (Asset : Carthage.Assets.Asset_Type) return Natural)
      is
         use Carthage.Assets;
      begin
         if To_List.Is_Empty then
            To_List.Append (Asset_Cursor);
         else
            declare
               Current_First : constant Asset_Type :=
                                 Managed_Asset_List.Element
                                   (To_List.First_Element).Asset;
               Current_Rating : constant Natural :=
                                  Rate (Current_First);
               New_Asset      : constant Asset_Type :=
                                  Managed_Asset_List.Element
                                    (Asset_Cursor).Asset;
               New_Rating     : constant Natural :=
                                  Rate (New_Asset);
            begin
               if New_Rating = Current_Rating then
                  To_List.Append (Asset_Cursor);
               elsif New_Rating > Current_Rating then
                  To_List.Clear;
                  To_List.Append (Asset_Cursor);
               end if;
            end;
         end if;
      end Add_Asset_Classification;

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

         Stack.Update.Set_Manager (Manager);

         for I in 1 .. Stack.Count loop
            Manager.Assets.Append
              (Managed_Asset_Record'
                 (Asset => Stack.Asset (I),
                  Stack => Manager.Stacks.Last,
                  Tile  => Stack.Tile));
         end loop;
      end Add_Stack;

   begin
      Manager.Planet.Scan_Stacks
        (Manager.House,
         Add_Stack'Access);

      declare
         use Carthage.Assets;
         function Rate_Movement (Asset : Asset_Type) return Natural
         is (Asset.Movement);

         function Rate_Spot (Asset : Asset_Type) return Natural
         is (Asset.Spot);
      begin
         for Position in Manager.Assets.Iterate loop
            Add_Asset_Classification
              (Position, Manager.Movers,
               Rate_Movement'Access);
            Add_Asset_Classification
              (Position, Manager.Spotters,
               Rate_Spot'Access);
         end loop;
      end;

   end Load_Initial_State;

   ------------------------
   -- On_Hostile_Spotted --
   ------------------------

   overriding procedure On_Hostile_Spotted
     (Manager : in out Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
   begin
      Manager.House.Log
        (Stack.Identifier & " spotted hostile " & Hostile.Identifier
         & " at " & Hostile.Tile.Description);
   end On_Hostile_Spotted;

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

   ------------------------
   -- Transfer_Resources --
   ------------------------

   procedure Transfer_Resources
     (Manager   : in out Asset_Manager_Record;
      Resources : in out Carthage.Resources.Stock_Interface'Class)
   is null;

end Carthage.Managers.Assets;