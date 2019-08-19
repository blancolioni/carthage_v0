with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Units;
with Carthage.Worlds;

package body Carthage.Stacks is

   type Stack_Add_Asset_Update is
     new Db.Root_Element_Update with
      record
         Asset : Carthage.Assets.Asset_Type;
      end record;

   overriding procedure Update_Element
     (Update  : Stack_Add_Asset_Update;
      Element : not null access Stack_Record'Class);

   type Stack_Remove_Asset_Update is
     new Db.Root_Element_Update with
      record
         Asset : Carthage.Assets.Asset_Type;
      end record;

   overriding procedure Update_Element
     (Update  : Stack_Remove_Asset_Update;
      Element : not null access Stack_Record'Class);

   type Stack_Remove_Dead_Assets_Update is
     new Db.Root_Element_Update with
      record
         null;
      end record;

   overriding procedure Update_Element
     (Update  : Stack_Remove_Dead_Assets_Update;
      Element : not null access Stack_Record'Class);

   type Stack_Move_To_Tile_Update is
     new Db.Root_Element_Update with
      record
         Tile  : Carthage.Tiles.Tile_Type;
      end record;

   overriding procedure Update_Element
     (Update  : Stack_Move_To_Tile_Update;
      Element : not null access Stack_Record'Class);

   ---------------
   -- Add_Asset --
   ---------------

   overriding procedure Add_Asset
     (To    : Stack_Record;
      Asset : Carthage.Assets.Asset_Type)
   is
      Update : Stack_Add_Asset_Update;
   begin
      Update.Set_Target (To);
      Update.Asset := Asset;
      Carthage.Objects.Add_Object_Update (Update);
   end Add_Asset;

   --------------------
   -- Clear_Property --
   --------------------

   procedure Clear_Property
     (Stack    : in out Stack_Record'Class;
      Property : Stack_Property)
   is
   begin
      Stack.Properties (Property) := False;
   end Clear_Property;

   ---------------
   -- Find_Path --
   ---------------

   function Find_Path
     (Stack   : Stack_Record'Class;
      Tile    : Carthage.Tiles.Tile_Type)
      return Array_Of_Positions
   is
      function Passable
        (Tile : Carthage.Tiles.Tile_Type)
                  return Boolean
      is (Stack.Movement_Cost (Tile) > 0.0);

      function Move_Cost
        (Tile : Carthage.Tiles.Tile_Type)
                  return Float
      is (Stack.Movement_Cost (Tile));

   begin
      return Stack.Planet.Find_Path
        (Start    => Stack.Tile.Position,
         Finish   => Tile.Position,
         Passable => Passable'Access,
         Cost     => Move_Cost'Access);
   end Find_Path;

   overriding function Log_Identifier
     (Stack : Stack_Record)
      return String
   is (Carthage.Objects.Root_Named_Object (Stack).Log_Identifier
       & "-"
       & (if Stack.Count = 0 then "EMPTY"
          else Stack.Assets (1).Identifier)
       & (if Stack.Count = 1 then "" else " ..."));

   ------------------
   -- Move_To_Tile --
   ------------------

   procedure Move_To_Tile
     (Stack : Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type)
   is
      Update : Stack_Move_To_Tile_Update;
   begin
      Update.Set_Target (Stack);
      Update.Tile := Tile;
      Carthage.Objects.Add_Object_Update (Update);
   end Move_To_Tile;

   --------------
   -- Movement --
   --------------

   function Movement
     (Stack : Stack_Record)
      return Natural
   is
   begin
      if Stack.Count = 0 then
         return 0;
      else
         return Result : Natural := Stack.Asset (1).Movement do
            for Index in 2 .. Stack.Count loop
               declare
                  M : constant Natural := Stack.Asset (Index).Movement;
               begin
                  if M < Result then
                     Result := M;
                  end if;
               end;
            end loop;
         end return;
      end if;
   end Movement;

   -------------------
   -- Movement_Cost --
   -------------------

   function Movement_Cost
     (Stack : Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type)
      return Float
   is
      World   : constant Carthage.Worlds.World_Type :=
                  Stack.Planet.Category;
      Terrain : constant Carthage.Tiles.Terrain_Layer_Array :=
                  Tile.Terrain;
      Lowest  : Float := Float'Last;
      Road    : constant Boolean := Tile.Has_Road;
   begin
      if Stack.Count = 0 then
         return 0.0;
      end if;

      for I in 1 .. Stack.Count loop
         declare
            Category : constant Carthage.Units.Unit_Category :=
                         Stack.Asset (I).Unit.Category;
            Cost     : Float := 1.0;
         begin
            for T of Terrain loop
               Cost := Cost * World.Movement_Multiplier (T, Category);
            end loop;
            if Road then
               Cost := Cost * World.Road_Movement_Multiplier (Category);
            end if;

            Lowest := Float'Min (Lowest, Cost);
            exit when Lowest = 0.0;
         end;
      end loop;

      return Lowest;
   end Movement_Cost;

   -----------------------
   -- Movement_Duration --
   -----------------------

   function Movement_Duration
     (Stack : Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type)
      return Duration
   is
   begin
      return Duration (7.0 * 24.0 * 60.0 * 60.0
                       * Stack.Movement_Cost (Tile)
                       / Float (Stack.Movement));
   end Movement_Duration;

   -----------------------
   -- Movement_Progress --
   -----------------------

   function Movement_Progress
     (Stack : Stack_Record)
      return Float
   is
      use Carthage.Calendar;
      D : constant Duration :=
            Clock - Stack.Next_Tile_Start;
   begin
      return Float (D) / Float (Stack.Next_Tile_Duration);
   end Movement_Progress;

   ------------------
   -- Remove_Asset --
   ------------------

   overriding procedure Remove_Asset
     (From  : Stack_Record;
      Asset : Carthage.Assets.Asset_Type)
   is
      Update : Stack_Remove_Asset_Update;
   begin
      Update.Set_Target (From);
      Update.Asset := Asset;
      Carthage.Objects.Add_Object_Update (Update);
   end Remove_Asset;

   ------------------------
   -- Remove_Dead_Assets --
   ------------------------

   procedure Remove_Dead_Assets
     (Stack : Stack_Record)
   is
      Update : Stack_Remove_Dead_Assets_Update;
   begin
      Update.Set_Target (Stack);
      Carthage.Objects.Add_Object_Update (Update);
   end Remove_Dead_Assets;

   --------------------------------
   -- Remove_Empty_Ground_Stacks --
   --------------------------------

   procedure Remove_Empty_Ground_Stacks is

      package Deleted_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Memor.Database_Reference, Memor."=");

      Deleted : Deleted_Lists.List;

      procedure Check_Stack
        (Stack : not null access Stack_Record'Class);

      -----------------
      -- Check_Stack --
      -----------------

      procedure Check_Stack
        (Stack : not null access Stack_Record'Class)
      is
         Back : Asset_Count := 0;
      begin
         for I in 1 .. Stack.Count loop
            if not Stack.Asset (I).Alive then
               Back := Back + 1;
            elsif Back > 0 then
               Stack.Assets (I - Back) := Stack.Assets (I);
            end if;
         end loop;
         Stack.Count := Stack.Count - Back;
         if Stack.Has_Tile and then Stack.Count = 0 then
            Stack.Log ("deleting empty " & Stack.Owner.Name & " stack");
            Stack.Tile.Update.Remove_Stack (Stack);
            if Stack.Manager /= null then
               Stack.Manager.On_Stack_Removed (Stack_Type (Stack));
            end if;
            Deleted.Append (Stack.Reference);
         end if;
      end Check_Stack;

   begin
      Db.Iterate (Check_Stack'Access);
      for Ref of Deleted loop
         Db.Delete (Ref);
      end loop;
   end Remove_Empty_Ground_Stacks;

   -----------------
   -- Scan_Stacks --
   -----------------

   procedure Scan_Stacks
     (Process : not null access procedure (Stack : Stack_Type))
   is
      function Real_Stack (Stack : Stack_Type) return Boolean
      is (Stack.Count > 0);

   begin
      Db.Scan (Real_Stack'Access, Process);
   end Scan_Stacks;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (Stack   : in out Stack_Record;
      Manager : not null access Stack_Manager_Interface'Class)
   is
   begin
      Stack.Manager := Manager;
   end Set_Manager;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Stack    : in out Stack_Record'Class;
      Property : Stack_Property)
   is
   begin
      Stack.Properties (Property) := True;
   end Set_Property;

   ----------
   -- Spot --
   ----------

   function Spot
     (Stack : Stack_Record)
      return Natural
   is
   begin
      if Stack.Count = 0 then
         return 0;
      else
         return Result : Natural := Stack.Asset (1).Unit.Spot do
            for Index in 2 .. Stack.Count loop
               declare
                  D : constant Natural := Stack.Asset (Index).Unit.Spot;
               begin
                  if D > Result then
                     Result := D;
                  end if;
               end;
            end loop;
         end return;
      end if;
   end Spot;

   -----------
   -- Total --
   -----------

   function Total
     (Stack : Stack_Record;
      Value : not null access
        function (Asset : Carthage.Assets.Asset_Type) return Integer)
      return Integer
   is
   begin
      return T : Integer := 0 do
         for I in 1 .. Stack.Count loop
            T := T + Value (Stack.Asset (I));
         end loop;
      end return;
   end Total;

   --------------------
   -- Total_Strength --
   --------------------

   function Total_Strength
     (Stack : Stack_Record'Class)
      return Natural
   is
      function Asset_Strength
        (Asset : Carthage.Assets.Asset_Type)
         return Integer;

      --------------------
      -- Asset_Strength --
      --------------------

      function Asset_Strength
        (Asset : Carthage.Assets.Asset_Type)
         return Integer
      is
         use Carthage.Units;
      begin
         return Strength : Integer := 0 do
            for Weapon in Weapon_Category loop
               if Asset.Unit.Has_Attack (Weapon) then
                  Strength := Strength + Asset.Unit.Strength (Weapon);
               end if;
            end loop;
         end return;
      end Asset_Strength;

   begin
      return Stack.Total (Asset_Strength'Access);
   end Total_Strength;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Stack_Record'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

   --------------------
   -- Update_Element --
   --------------------

   overriding procedure Update_Element
     (Update  : Stack_Add_Asset_Update;
      Element : not null access Stack_Record'Class)
   is
   begin
      Element.Count := Element.Count + 1;
      Element.Assets (Element.Count) := Update.Asset;
   end Update_Element;

   --------------------
   -- Update_Element --
   --------------------

   overriding procedure Update_Element
     (Update  : Stack_Remove_Asset_Update;
      Element : not null access Stack_Record'Class)
   is
      use type Carthage.Assets.Asset_Type;
      Found : Boolean := False;
   begin
      for I in 1 .. Element.Count loop
         if Element.Assets (I) = Update.Asset then
            Found := True;
         elsif Found then
            Element.Assets (I - 1) := Element.Assets (I);
         end if;
      end loop;
      if not Found then
         raise Program_Error with
         Element.Identifier
           & ": expected to find asset: " & Update.Asset.Identifier;
      end if;
      Element.Count := Element.Count - 1;
   end Update_Element;

   --------------------
   -- Update_Element --
   --------------------

   overriding procedure Update_Element
     (Update  : Stack_Remove_Dead_Assets_Update;
      Element : not null access Stack_Record'Class)
   is
      pragma Unreferenced (Update);
      Target : Asset_Index := 1;
   begin
      for I in 1 .. Element.Count loop
         if Element.Assets (I).Alive then
            if Target < I then
               Element.Assets (Target) := Element.Assets (I);
            end if;
            Target := Target + 1;
         else
            null;
         end if;
      end loop;
      Element.Assets (Target .. Element.Assets'Last) := (others => null);
      Element.Count := Target - 1;
   end Update_Element;

   --------------------
   -- Update_Element --
   --------------------

   overriding procedure Update_Element
     (Update  : Stack_Move_To_Tile_Update;
      Element : not null access Stack_Record'Class)
   is
   begin
      Element.Log ("moving to " & Update.Tile.Description);
      Element.Orders.Append
        (Stack_Order_Record'
           (Order_Type    => Move_To_Tile,
            Destination   => Update.Tile.Position));
   end Update_Element;

end Carthage.Stacks;
