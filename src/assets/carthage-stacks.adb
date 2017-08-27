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

end Carthage.Stacks;
