package body Carthage.Assets is

   type Asset_Set_Container_Update is
     new Db.Root_Element_Update with
      record
         Container : Asset_Container_Type;
      end record;

   overriding procedure Update_Element
     (Update  : Asset_Set_Container_Update;
      Element : not null access Asset_Record'Class);

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

   ------------------
   -- Delete_Asset --
   ------------------

   procedure Delete_Asset (Asset : Asset_Type) is
   begin
      Db.Delete (Asset.Reference);
   end Delete_Asset;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Asset     : not null access constant Asset_Record'Class;
      Container : not null access constant Asset_Container_Interface'Class)
   is
   begin
      declare
         Update : Asset_Set_Container_Update;
      begin
         Update.Set_Target (Asset);
         Update.Container := Asset_Container_Type (Container);
         Carthage.Objects.Add_Object_Update (Update);
      end;

      Container.Add_Asset (Asset_Type (Asset));

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

   --------------------
   -- Update_Element --
   --------------------

   overriding procedure Update_Element
     (Update  : Asset_Set_Container_Update;
      Element : not null access Asset_Record'Class)
   is
   begin
      Element.Container := Update.Container;
   end Update_Element;

end Carthage.Assets;
