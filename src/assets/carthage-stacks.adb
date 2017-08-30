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
   begin
      Db.Scan (Process);
   end Scan_Stacks;

end Carthage.Stacks;
