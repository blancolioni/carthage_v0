with Carthage.Assets.Create;
with Carthage.Assets.Moves;
with Carthage.Galaxy;
with Carthage.Stacks.Create;
with Carthage.Stacks.Updates;
with Carthage.Units;

package body Carthage.Managers.Assets is

   type Ground_Asset_Manager_Record is
     new Root_Asset_Manager_Record with
      record
         Planet       : Carthage.Planets.Planet_Type;
         Stacks       : Managed_Stack_List.List;
         Stack_Maps   : Managed_Stack_Maps.Map;
      end record;

   type Ground_Asset_Manager_Type is
     access all Ground_Asset_Manager_Record'Class;

   overriding function Description
     (Manager : Ground_Asset_Manager_Record)
      return String
   is (Manager.House.Name & " " & Manager.Planet.Name
       & " ground asset manager");

   overriding procedure Load_Assets
     (Manager : in out Ground_Asset_Manager_Record);

   overriding procedure On_Hostile_Spotted
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class;
      Stop    : out Boolean);

   overriding procedure On_Hostile_Spotted
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class)
   is null;

   overriding procedure On_Stack_Removed
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class);

   overriding procedure On_Movement_Ended
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class);

   overriding function Have_Immediate_Capacity
     (Manager : Ground_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   overriding function Check_Goal
     (Manager : not null access Ground_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   type Space_Asset_Manager_Record is
     new Root_Asset_Manager_Record with
      record
         null;
      end record;

   type Space_Asset_Manager_Type is
     access all Space_Asset_Manager_Record'Class;

   overriding function Description
     (Manager : Space_Asset_Manager_Record)
      return String
   is (Manager.House.Name
       & " space asset manager");

   overriding function Have_Immediate_Capacity
     (Manager : Space_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   overriding function Check_Goal
     (Manager : not null access Space_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   overriding procedure Load_Assets
     (Manager : in out Space_Asset_Manager_Record);

   overriding procedure On_Hostile_Spotted
     (Manager : in out Space_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class;
      Stop    : out Boolean)
   is null;

   overriding procedure On_Hostile_Spotted
     (Manager : in out Space_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class)
   is null;

   ----------------
   -- Check_Goal --
   ----------------

   overriding function Check_Goal
     (Manager : not null access Ground_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
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
               if Managed_Stack.Goal.Is_Empty
                 and then Managed_Stack.Stack.Can_Enter
                   (Manager.Planet.Tile (Target))
               then
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
            return True;

         when Recon =>
            declare
               use type Carthage.Stacks.Asset_Count;
               Asset_Cursor : constant Managed_Asset_List.Cursor :=
                                Best_Available (Manager.Spotters,
                                                Asset_Goal.Tile.Position);
            begin
               if not Managed_Asset_List.Has_Element (Asset_Cursor) then
                  return False;
               end if;

               declare
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
                                        (Manager, Stack.Owner, Stack.Planet,
                                         Stack.Tile);
                     begin
                        Asset.Move_To (New_Stack);
                        Manager.Stacks.Append
                          ((New_Stack, others => <>));
                        Stack_Cursor := Manager.Stacks.Last;
                        Manager.Assets (Asset_Cursor).Stack := Stack_Cursor;
                        Manager.Stack_Maps.Insert (New_Stack.Identifier,
                                                   Stack_Cursor);
                        Stack := New_Stack;
                     end;
                  end if;

                  Asset.Log ("assigned to recon of "
                             & Asset_Goal.Tile.Description);
                  Manager.Stacks (Stack_Cursor).Goal.Replace_Element
                    (Asset_Goal);
                  Manager.Stacks (Stack_Cursor).Stack.Update.Move_To_Tile
                    (Asset_Goal.Tile);
                  Carthage.Stacks.Updates.Start_Update
                    (Manager.Stacks (Stack_Cursor).Stack);
                  return True;
               end;
            end;

         when Capture =>
            declare
               Closest_Stack : Managed_Stack_List.Cursor :=
                                 Managed_Stack_List.No_Element;
               Smallest_D    : Natural := Natural'Last;
               Current_Str   : Natural := 0;
               Required_Str  : constant Natural :=
                                 Asset_Goal.Parameters.Strength;
            begin
               Manager.House.Log ("checking capture goal; required strength"
                                  & Natural'Image (Required_Str));

               for Position in Manager.Stacks.Iterate loop
                  declare
                     use type Carthage.Tiles.Tile_Type;
                     Stack : constant Carthage.Stacks.Stack_Type :=
                               Manager.Stacks (Position).Stack;
                     Str   : constant Natural :=
                               Stack.Total_Strength;
                     D     : constant Natural :=
                               (if Stack.Tile = null
                                then Natural'Last
                                else Carthage.Planets.Hex_Distance
                                  (Stack.Tile.Position,
                                   Asset_Goal.Tile.Position));
                  begin
                     if Stack.Tile = null then
                        Stack.Log ("no tile: " & Stack.Description);
                     elsif Manager.Stacks (Position).Goal.Is_Empty then
                        if (D < Smallest_D
                            and then Str >= Required_Str)
                          or else (D / 2 < Smallest_D
                                   and then Str >= Required_Str
                                   and then Str * 2 / 3 > Current_Str)
                        then
                           Stack.Log
                             ("strength" & Str'Img & "; distance" & D'Img);
                           declare
                              Path : constant Array_Of_Positions :=
                                       Stack.Find_Path
                                         (Asset_Goal.Tile);
                           begin
                              if Stack.Has_Property
                                (Carthage.Stacks.Defender)
                              then
                                 Stack.Log ("skipping defender");
                              elsif Path'Length > 1 then
                                 Closest_Stack := Position;
                                 Smallest_D := D;
                                 Current_Str := Str;
                              else
                                 Stack.Log ("no path");
                              end if;
                           end;
                        end if;
                     end if;
                  end;
               end loop;

               if Managed_Stack_List.Has_Element (Closest_Stack) then
                  declare
                     Stack : constant Carthage.Stacks.Stack_Type :=
                               Managed_Stack_List.Element
                                 (Closest_Stack).Stack;
                  begin
                     Stack.Log (Stack.Description
                                & ": assigned to capture of "
                                & Asset_Goal.Tile.Description);
                     for I in 1 .. Stack.Count loop
                        Stack.Log ("asset: " & Stack.Asset (I).Identifier
                                   & " move"
                                   & Natural'Image (Stack.Asset (I).Movement));
                     end loop;

                     Manager.Stacks (Closest_Stack).Goal.Replace_Element
                       (Asset_Goal);
                     Manager.Stacks (Closest_Stack).Stack.Update.Move_To_Tile
                       (Asset_Goal.Tile);
                     Carthage.Stacks.Updates.Start_Update (Stack);
                  end;
                  return True;
               else
                  return False;
               end if;
            end;

         when Transfer =>

            declare
               Asset : constant Carthage.Assets.Asset_Type :=
                 Carthage.Assets.Create.New_Asset
                   (Unit    => Carthage.Units.Cargo_Pod,
                    Owner   => Asset_Goal.City_1.Owner);
               Stack : constant Carthage.Stacks.Stack_Type :=
                 Carthage.Stacks.Create.New_Ground_Stack
                   (Manager, Asset.Owner, Manager.Planet,
                    Asset_Goal.City_1.Tile);
               Stack_Cursor : Managed_Stack_List.Cursor;
            begin
               Manager.Stacks.Append
                 ((Stack, others => <>));
               Stack_Cursor := Manager.Stacks.Last;

               Manager.Assets.Append
                 (Managed_Asset_Record'
                    (Asset  => Asset,
                     Stack  => Stack_Cursor,
                     Planet => Manager.Planet,
                     Tile   => Asset_Goal.City_1.Tile,
                     Goal   =>
                       Asset_Manager_Goal_Holders.To_Holder (Asset_Goal)));
               Asset.Move_To (Stack);
               Manager.Stack_Maps.Insert
                 (Stack.Identifier, Stack_Cursor);

               Asset.Log ("assigned to transfer "
                          & Asset_Goal.Resource.Name
                          & " from "
                          & Asset_Goal.City_1.Name
                          & " to "
                          & Asset_Goal.City_2.Name);

               Manager.Stacks (Stack_Cursor).Goal.Replace_Element
                 (Asset_Goal);
               Manager.Stacks (Stack_Cursor).Stack.Move_To_Tile
                 (Asset_Goal.City_2.Tile);
               Carthage.Stacks.Updates.Start_Update
                 (Manager.Stacks (Stack_Cursor).Stack);
               return True;
            end;

      end case;
   end Check_Goal;

   ----------------
   -- Check_Goal --
   ----------------

   overriding function Check_Goal
     (Manager : not null access Space_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);

      function Best_Available
        (Target : Carthage.Planets.Planet_Type)
         return Managed_Asset_List.Cursor;

      --------------------
      -- Best_Available --
      --------------------

      function Best_Available
        (Target : Carthage.Planets.Planet_Type)
         return Managed_Asset_List.Cursor
      is
         Best_Asset    : Managed_Asset_List.Cursor :=
                           Managed_Asset_List.No_Element;
         Best_Distance : Natural := Natural'Last;

      begin
         for Cursor in Manager.Assets.Iterate loop
            declare
               Managed_Asset : Managed_Asset_Record renames
                                 Manager.Assets (Cursor);
            begin
               if Managed_Asset.Goal.Is_Empty then
                  declare
                     Asset : constant Carthage.Assets.Asset_Type :=
                               Managed_Asset.Asset;
                     Current  : constant Carthage.Planets.Planet_Type :=
                                  Carthage.Stacks.Stack_Type
                                    (Asset.Container).Planet;
                     Launch_Cost : constant Natural :=
                                     (if Asset.Container.Is_Orbiting
                                      then 0 else 1);
                     Distance : constant Natural :=
                                  Carthage.Galaxy.Jump_Count
                                    (Current, Target)
                                    + Launch_Cost;
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
            return True;
         when Recon =>
            declare
               Asset_Cursor : constant Managed_Asset_List.Cursor :=
                                Best_Available (Asset_Goal.Planet);
            begin
               if not Managed_Asset_List.Has_Element (Asset_Cursor) then
                  return False;
               end if;

               declare
                  Asset        : constant Carthage.Assets.Asset_Type :=
                                   Managed_Asset_List.Element
                                     (Asset_Cursor).Asset;
                  Planet       : constant Carthage.Planets.Planet_Type :=
                                   Carthage.Stacks.Stack_Type
                                     (Asset.Container)
                                     .Planet;
                  procedure Set_Goal
                    (Item : in out Managed_Asset_Record);

                  --------------
                  -- Set_Goal --
                  --------------

                  procedure Set_Goal
                    (Item : in out Managed_Asset_Record)
                  is
                  begin
                     Item.Goal.Replace_Element (Asset_Goal);
                  end Set_Goal;

               begin
                  Asset.Log ("assigned to recon of "
                             & Asset_Goal.Planet.Name);
                  Manager.Assets.Update_Element
                    (Asset_Cursor,
                     Set_Goal'Access);
                  Carthage.Assets.Moves.Start_Jump
                    (Asset, Planet, Asset_Goal.Planet);
                  return True;
               end;
            end;

         when Capture =>
            return True;

         when Transfer =>
            return True;

      end case;
   end Check_Goal;

   -------------------------------
   -- Get_Resource_Requirements --
   -------------------------------

   overriding procedure Get_Resource_Requirements
     (Manager : in out Root_Asset_Manager_Record;
      Minimum : in out Carthage.Resources.Stock_Interface'Class;
      Desired : in out Carthage.Resources.Stock_Interface'Class)
   is
      Food : constant Carthage.Resources.Resource_Type :=
               Carthage.Resources.Food;
      Minimum_Food : Resource_Quantity := 0.0;
      Desired_Food : Resource_Quantity;
   begin

      for Managed_Asset of Manager.Assets loop
         Minimum_Food := Minimum_Food + Managed_Asset.Asset.Unit.Eat;
      end loop;

      Desired_Food := Minimum_Food * 2.0;

      Minimum.Add (Food, Minimum_Food / 20.0);
      Desired.Add (Food, Desired_Food / 20.0);

   end Get_Resource_Requirements;

   --------------------------
   -- Ground_Asset_Manager --
   --------------------------

   function Ground_Asset_Manager
     (Meta_Manager : Stack_Meta_Manager_Access;
      House        : Carthage.Houses.House_Type;
      Planet       : Carthage.Planets.Planet_Type)
      return Manager_Type
   is
      Manager : constant Ground_Asset_Manager_Type :=
                  new Ground_Asset_Manager_Record;
   begin
      Manager.Meta_Manager := Meta_Manager;
      Manager.House := House;
      Manager.Planet := Planet;
      Manager.Initialize;

      for Stack of Manager.Stacks loop
         Stack.Stack.Update.Set_Manager (Manager);
      end loop;

      Add_Manager (Manager);

      return Manager_Type (Manager);
   end Ground_Asset_Manager;

   -----------------------------
   -- Have_Immediate_Capacity --
   -----------------------------

   overriding function Have_Immediate_Capacity
     (Manager : Ground_Asset_Manager_Record;
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
               if Managed_Stack.Goal.Is_Empty
                 and then Managed_Stack.Stack.Can_Enter (Asset_Goal.Tile)
               then
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
         when Transfer =>
            return True;
      end case;
   end Have_Immediate_Capacity;

   -----------------------------
   -- Have_Immediate_Capacity --
   -----------------------------

   overriding function Have_Immediate_Capacity
     (Manager : Space_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);

   begin
      case Asset_Goal.Class is
         when None =>
            return True;
         when Recon =>
            for Managed_Asset of Manager.Assets loop
               if Managed_Asset.Goal.Is_Empty then
                  return True;
               end if;
            end loop;
            return False;
         when Capture =>
            return False;
         when Transfer =>
            return False;
      end case;
   end Have_Immediate_Capacity;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : not null access Root_Asset_Manager_Record)
   is

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
         New_Asset      : constant Asset_Type :=
           Managed_Asset_List.Element
             (Asset_Cursor).Asset;
         New_Rating     : constant Natural :=
           Rate (New_Asset);
      begin
         if New_Rating = 0 then
            null;
         elsif To_List.Is_Empty then
            To_List.Append (Asset_Cursor);
         else
            declare
               Current_First  : constant Asset_Type :=
                                  Managed_Asset_List.Element
                                    (To_List.First_Element).Asset;
               Current_Rating : constant Natural :=
                                  Rate (Current_First);
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

   begin

      Root_Asset_Manager_Record'Class (Manager.all).Load_Assets;

      declare
         use Carthage.Assets;
         function Rate_Movement (Asset : Asset_Type) return Natural
         is (if Asset.Ground_Asset then Asset.Movement else 0);

         function Rate_Spot (Asset : Asset_Type) return Natural
         is (if Asset.Ground_Asset then Asset.Spot else 0);

         function Rate_Ground_Cargo (Asset : Asset_Type) return Natural
         is (if Asset.Ground_Asset then Asset.Cargo_Capacity else 0);

      begin
         for Position in Manager.Assets.Iterate loop
            Add_Asset_Classification
              (Position, Manager.Movers,
               Rate_Movement'Access);
            Add_Asset_Classification
              (Position, Manager.Spotters,
               Rate_Spot'Access);
            Add_Asset_Classification
              (Position, Manager.Ground_Cargo,
               Rate_Ground_Cargo'Access);
         end loop;
      end;

   end Initialize;

   -----------------
   -- Load_Assets --
   -----------------

   overriding procedure Load_Assets
     (Manager : in out Ground_Asset_Manager_Record)
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
              (Stack        => Carthage.Stacks.Stack_Type (Stack),
               Goal         => <>));

         Manager.Stack_Maps.Insert
           (Stack.Identifier, Manager.Stacks.Last);

         for I in 1 .. Stack.Count loop
            Manager.Assets.Append
              (Managed_Asset_Record'
                 (Asset  => Stack.Asset (I),
                  Stack  => Manager.Stacks.Last,
                  Planet => Stack.Planet,
                  Tile   => Stack.Tile,
                  Goal   => <>));

            if not Stack.Has_Property (Carthage.Stacks.Defender)
              and then (Stack.Asset (I).Unit.Is_Sceptre
                        or else Stack.Asset (I).Unit.Is_Noble)
            then
               Stack.Update.Set_Property (Carthage.Stacks.Defender);
            end if;
         end loop;
      end Add_Stack;

   begin
      Manager.Planet.Scan_Stacks
        (Manager.House,
         Add_Stack'Access);
   end Load_Assets;

   -----------------
   -- Load_Assets --
   -----------------

   overriding procedure Load_Assets
     (Manager : in out Space_Asset_Manager_Record)
   is
      procedure Add_Assets
        (Planet : Carthage.Planets.Planet_Type);

      ----------------
      -- Add_Assets --
      ----------------

      procedure Add_Assets
        (Planet : Carthage.Planets.Planet_Type)
      is
         Stack : constant Carthage.Stacks.Stack_Type :=
                   Carthage.Stacks.Stack_Type
                     (Planet.Orbital_Stack (Manager.House));

         procedure Add_Space_Assets
           (Stack : not null access constant
              Carthage.Stacks.Stack_Class);

         ----------------------
         -- Add_Space_Assets --
         ----------------------

         procedure Add_Space_Assets
           (Stack : not null access constant
              Carthage.Stacks.Stack_Class)
         is
         begin
            for Asset_Index in 1 .. Stack.Count loop
               if Stack.Asset (Asset_Index).Space_Asset then
                  Manager.Assets.Append
                    (Managed_Asset_Record'
                       (Asset  => Stack.Asset (Asset_Index),
                        Stack  => Managed_Stack_List.No_Element,
                        Planet => Planet,
                        Tile   => Stack.Tile,
                        Goal   => <>));
               end if;
            end loop;
         end Add_Space_Assets;

      begin
         if not Stack.Is_Empty then
            for Index in 1 .. Stack.Count loop
               Manager.Assets.Append
                 (Managed_Asset_Record'
                    (Asset  => Stack.Asset (Index),
                     Stack  => Managed_Stack_List.No_Element,
                     Planet => Planet,
                     Tile   => null,
                     Goal   => <>));
            end loop;
         end if;
         Planet.Scan_Stacks (Manager.House, Add_Space_Assets'Access);
      end Add_Assets;

   begin
      Carthage.Planets.Scan (Add_Assets'Access);
   end Load_Assets;

   ------------------------
   -- On_Hostile_Spotted --
   ------------------------

   overriding procedure On_Hostile_Spotted
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class;
      Hostile : not null access constant Carthage.Stacks.Stack_Record'Class;
      Stop    : out Boolean)
   is
      use Carthage.Stacks;
   begin
      Manager.House.Log
        (Stack.Identifier & " spotted hostile " & Hostile.Identifier
         & " at " & Hostile.Tile.Description);
      Stop := False;

      for S of Manager.Stacks loop
         if S.Stack = Stack then
            if not S.Goal.Is_Empty
              and then S.Goal.Element.Class /= Capture
            then
               Stack.Log ("stopping because goal class is "
                          & Goal_Class'Image (S.Goal.Element.Class));
               S.Goal.Clear;
               Stop := True;
            end if;
            exit;
         end if;
      end loop;

      Manager.Meta_Manager.On_Hostile_Spotted (Stack, Hostile);
   end On_Hostile_Spotted;

   -----------------------
   -- On_Movement_Ended --
   -----------------------

   overriding procedure On_Movement_Ended
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
      Position : constant Managed_Stack_List.Cursor :=
        Manager.Stack_Maps.Element (Stack.Identifier);
      Rec      : Managed_Stack_Record renames Manager.Stacks (Position)
        with Unreferenced;
   begin
      Stack.Log ("movement ended");
   end On_Movement_Ended;

   ----------------------
   -- On_Stack_Removed --
   ----------------------

   overriding procedure On_Stack_Removed
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : not null access constant Carthage.Stacks.Stack_Record'Class)
   is
      Position : Managed_Stack_List.Cursor :=
                   Manager.Stack_Maps.Element (Stack.Identifier);
   begin
      Manager.Stacks.Delete (Position);
      Manager.Stack_Maps.Delete (Stack.Identifier);
   end On_Stack_Removed;

   --------------------------------
   -- Planet_Reconnaissance_Goal --
   --------------------------------

   function Planet_Reconnaissance_Goal
     (Planet : Carthage.Planets.Planet_Type)
      return Carthage.Goals.Goal_Record'Class
   is
      Goal : constant Asset_Manager_Goal :=
               Asset_Manager_Goal'
                 (Carthage.Goals.Goal_Record with
                  Priority   => Default_Priority (Recon),
                  Class      => Recon,
                  Planet     => Planet,
                  Parameters => (Speed => High, Spot => High, others => <>),
                  others     => <>);
   begin
      return Goal;
   end Planet_Reconnaissance_Goal;

   -------------------------
   -- Space_Asset_Manager --
   -------------------------

   function Space_Asset_Manager
     (Meta_Manager : Stack_Meta_Manager_Access;
      House        : Carthage.Houses.House_Type)
      return Manager_Type
   is
      Manager : constant Space_Asset_Manager_Type :=
                  new Space_Asset_Manager_Record;
   begin
      Manager.Meta_Manager := Meta_Manager;
      Manager.House := House;
      Manager.Initialize;

      Add_Manager (Manager);

      return Manager_Type (Manager);
   end Space_Asset_Manager;

   -------------------
   -- Take_Resource --
   -------------------

   overriding procedure Take_Resource
     (From     : in out Root_Asset_Manager_Record;
      Resource : Carthage.Resources.Resource_Type;
      Quantity : in out Resource_Quantity)
   is
   begin
      Quantity :=
        Resource_Quantity'Min
          (Quantity, From.Resources.Quantity (Resource));
      From.Resources.Remove (Resource, Quantity);
   end Take_Resource;

   -----------------------
   -- Tile_Capture_Goal --
   -----------------------

   function Tile_Capture_Goal
     (Tile     : Carthage.Tiles.Tile_Type;
      Strength : Natural)
      return Carthage.Goals.Goal_Record'Class
   is
      Goal : constant Asset_Manager_Goal :=
               Asset_Manager_Goal'
                 (Carthage.Goals.Goal_Record with
                  Priority   => Default_Priority (Capture),
                  Class      => Capture,
                  Planet     => null,
                  Tile       => Tile,
                  City_1     => null,
                  City_2     => null,
                  Resource   => null,
                  Parameters =>
                    (Speed    => Low, Spot => Low, Cargo => Low,
                     Military => High, Strength => Strength));
   begin
      return Goal;
   end Tile_Capture_Goal;

   ------------------------------
   -- Tile_Reconnaissance_Goal --
   ------------------------------

   function Tile_Reconnaissance_Goal
     (Tile    : Carthage.Tiles.Tile_Type)
      return Carthage.Goals.Goal_Record'Class
   is
      Goal : constant Asset_Manager_Goal :=
               Asset_Manager_Goal'
                 (Carthage.Goals.Goal_Record with
                  Priority   => Default_Priority (Recon),
                  Class      => Recon,
                  Planet     => null,
                  Tile       => Tile,
                  City_1     => null,
                  City_2     => null,
                  Resource   => null,
                  Parameters => (Speed => High, Spot => High, others => <>));
   begin
      return Goal;
   end Tile_Reconnaissance_Goal;

   -------------------------
   -- Transfer_Cargo_Goal --
   -------------------------

   function Transfer_Cargo_Goal
     (From, To : Carthage.Cities.City_Type;
      Resource : Carthage.Resources.Resource_Type)
      return Carthage.Goals.Goal_Record'Class
   is
      Goal : constant Asset_Manager_Goal :=
        Asset_Manager_Goal'
          (Carthage.Goals.Goal_Record with
           Priority   => Default_Priority (Transfer),
           Class      => Transfer,
           Planet     => null,
           Tile       => From.Tile,
           City_1     => From,
           City_2     => To,
           Resource   => Resource,
           Parameters => (Cargo => High, Speed => Medium, others => <>));
   begin
      return Goal;
   end Transfer_Cargo_Goal;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Manager : not null access Root_Asset_Manager_Record)
      return Duration
   is
   begin
      Root_Asset_Manager_Record'Class (Manager.all).Check_Goals;
      return Root_Asset_Manager_Record'Class (Manager.all)
        .Average_Update_Frequency;
   end Update;

end Carthage.Managers.Assets;
