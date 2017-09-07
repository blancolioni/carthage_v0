package body Carthage.Stacks is

   ---------------
   -- Add_Asset --
   ---------------

   procedure Add_Asset
     (To    : Stack_Type;
      Asset : Carthage.Assets.Asset_Type)
   is
      procedure Update (Stack : in out Stack_Class);

      ------------
      -- Update --
      ------------

      procedure Update (Stack : in out Stack_Class) is
      begin
         Stack.Count := Stack.Count + 1;
         Stack.Assets (Stack.Count) := Asset;
      end Update;

   begin
      Db.Update (To.Reference, Update'Access);
   end Add_Asset;

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
   begin
      null;
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
