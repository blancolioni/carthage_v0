package body Carthage.Assets is

   ------------
   -- Damage --
   ------------

   procedure Damage
     (Asset  : in out Asset_Record;
      Points : Positive)
   is
   begin
      if Points >= Natural (Asset.Health) then
         Asset.Health := 0;
      else
         Asset.Health := Asset.Health - Health_Type (Points);
      end if;
   end Damage;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Asset     : not null access constant Asset_Record'Class;
      Container : not null access constant Asset_Container_Interface'Class)
   is
   begin
      if Asset.Container /= null then
         Asset.Container.Variable_Reference.Remove_Asset (Asset_Type (Asset));
      end if;

      Asset.Update.Container := Container;

      Asset.Container.Variable_Reference.Add_Asset (Asset_Type (Asset));

   end Move_To;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (Asset        : in out Asset_Record;
      Resource     : not null access constant
        Carthage.Resources.Resource_Class;
      New_Quantity : Resource_Quantity)
   is
   begin
      Asset.Stock.Set_Quantity (Resource, New_Quantity);
   end Set_Quantity;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant Asset_Record'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Carthage.Assets;
