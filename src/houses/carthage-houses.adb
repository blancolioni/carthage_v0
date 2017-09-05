package body Carthage.Houses is

   -----------
   -- Clear --
   -----------

   procedure Clear (Set : in out House_Set) is
   begin
      Set := 0;
   end Clear;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (House  : in out House_Record;
      Amount : Positive)
   is
   begin
      House.Cash := House.Cash + Amount;
   end Earn;

   ------------
   -- Insert --
   ------------

   procedure Insert (Set   : in out House_Set;
                     House : House_Type)
   is
   begin
      Set := Set or House.Set_Flag;
   end Insert;

   ----------------
   -- Log_Status --
   ----------------

   procedure Log_Status
     (House : House_Record'Class)
   is
   begin
      House.Log ("cash:" & House.Cash'Img & "; debt:" & House.Debt'Img);
   end Log_Status;

   ------------
   -- Remove --
   ------------

   procedure Remove (Set   : in out House_Set;
                     House : House_Type)
   is
   begin
      Set := Set and not House.Set_Flag;
   end Remove;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access procedure (House : House_Type))
   is
   begin
      Db.Scan (Process);
   end Scan;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access procedure
        (House : House_Record'Class))
   is
   begin
      Db.Scan (Process);
   end Scan;

   ------------------------
   -- Scan_Known_Planets --
   ------------------------

   procedure Scan_Known_Planets
     (House   : House_Record;
      Process : not null access
        procedure (Planet_Id : String))
   is
   begin
      for Id of House.Known_Planets loop
         Process (Id);
      end loop;
   end Scan_Known_Planets;

   -----------------------
   -- Set_House_Manager --
   -----------------------

   procedure Set_House_Manager
     (House   : House_Type;
      Manager : not null access
        Carthage.Managers.Manager_Record'Class)
   is
      procedure Update (Rec : in out House_Class);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out House_Class) is
      begin
         Rec.Manager := Manager;
      end Update;

   begin
      Db.Update (House.Reference, Update'Access);
   end Set_House_Manager;

   -----------
   -- Spend --
   -----------

   procedure Spend
     (House  : in out House_Record;
      Amount : Positive)
   is
   begin
      if Amount <= House.Cash then
         House.Cash := House.Cash - Amount;
      else
         House.Debt := House.Debt + Amount - House.Cash;
         House.Cash := 0;
      end if;
   end Spend;

   ------------
   -- Update --
   ------------

   function Update
     (Item : not null access constant House_Record'Class)
      return Updateable_Reference
   is
      Base_Update : constant Db.Updateable_Reference := Db.Update (Item);
   begin
      return Updateable_Reference'(Base_Update.Element, Base_Update);
   end Update;

end Carthage.Houses;
