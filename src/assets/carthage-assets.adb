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
         Asset.Health := Asset.Health - Asset_Health (Points);
      end if;
   end Damage;

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
