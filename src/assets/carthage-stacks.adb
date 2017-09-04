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

   ------------
   -- Update --
   ------------

   procedure Update
     (Stack  : Stack_Type;
      Update : not null access
        procedure (Rec : in out Stack_Class))
   is
   begin
      Db.Update (Stack.Reference, Update);
   end Update;

end Carthage.Stacks;
