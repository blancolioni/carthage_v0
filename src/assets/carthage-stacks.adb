with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Units;
with Carthage.Worlds;

package body Carthage.Stacks is

   ---------------
   -- Add_Asset --
   ---------------

   procedure Add_Asset
     (To    : in out Stack_Record;
      Asset : Carthage.Assets.Asset_Type)
   is
   begin
      To.Count := To.Count + 1;
      To.Assets (To.Count) := Asset;
   end Add_Asset;

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
      is (Stack.Movement_Cost (Tile) > 0);

      function Move_Cost
        (Tile : Carthage.Tiles.Tile_Type)
                  return Float
      is (Float (Stack.Movement_Cost (Tile)));

   begin
      return Stack.Planet.Find_Path
        (Start    => Stack.Tile.Position,
         Finish   => Tile.Position,
         Passable => Passable'Access,
         Cost     => Move_Cost'Access);
   end Find_Path;

   ------------------
   -- Move_To_Tile --
   ------------------

   procedure Move_To_Tile
     (Stack : in out Stack_Record;
      Tile  : Carthage.Tiles.Tile_Type)
   is
   begin
      Stack.Log ("moving to " & Tile.Description);
      Stack.Orders.Append
        (Stack_Order_Record'
           (Order_Type    => Move_To_Tile,
            Destination   => Tile.Position));
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
      return Natural
   is
      World  : constant Carthage.Worlds.World_Type :=
                 Stack.Planet.Category;
      Terrain : constant Carthage.Tiles.Terrain_Layer_Array := Tile.Terrain;
      Lowest  : Float := Float'Last;
      Road    : constant Boolean := Tile.Has_Road;
   begin
      if Stack.Count = 0 then
         return 0;
      end if;

      for I in 1 .. Stack.Count loop
         declare
            Category : constant Carthage.Units.Unit_Category :=
                         Stack.Asset (I).Unit.Category;
            Cost : Float := 1.0;
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

      if Lowest = 0.0 then
         return 0;
      elsif Float'Truncation (Lowest) = Lowest then
         return Natural (Lowest);
      else
         return Natural (Float'Truncation (Lowest)) + 1;
      end if;

   end Movement_Cost;

   ------------------
   -- Remove_Asset --
   ------------------

   procedure Remove_Asset
     (From  : in out Stack_Record;
      Asset : Carthage.Assets.Asset_Type)
   is
      use type Carthage.Assets.Asset_Type;
      Found : Boolean := False;
   begin
      for I in 1 .. From.Count loop
         if From.Assets (I) = Asset then
            Found := True;
         elsif Found then
            From.Assets (I - 1) := From.Assets (I);
         end if;
      end loop;
      if not Found then
         raise Program_Error with
           "assertion failed: " & From.Identifier
           & ": expected to find asset: " & Asset.Identifier;
      end if;
      From.Count := From.Count - 1;
   end Remove_Asset;

   ------------------------
   -- Remove_Dead_Assets --
   ------------------------

   procedure Remove_Dead_Assets
     (Stack : in out Stack_Record)
   is
      Target : Asset_Index := 1;
   begin
      for I in 1 .. Stack.Count loop
         if Stack.Assets (I).Alive then
            if Target < I then
               Stack.Assets (Target) := Stack.Assets (I);
            end if;
            Target := Target + 1;
         else
            null;
         end if;
      end loop;
      Stack.Assets (Target .. Stack.Assets'Last) := (others => null);
      Stack.Count := Target - 1;
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

end Carthage.Stacks;
