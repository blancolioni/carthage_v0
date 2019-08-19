package body Carthage.Resources is

   ---------
   -- Add --
   ---------

   procedure Add
     (Stock          : in out Stock_Interface'Class;
      Resource       : not null access constant Resource_Class;
      Added_Quantity : Resource_Quantity)
   is
   begin
      Stock.Set_Quantity
        (Resource, Stock.Quantity (Resource) + Added_Quantity);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Stock          : in out Stock_Interface'Class;
      Resource       : not null access constant Resource_Class;
      Added_Quantity : Natural)
   is
   begin
      Stock.Add (Resource, Resource_Quantity (Added_Quantity));
   end Add;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To    : in out Stock_Interface'Class;
      Stock : Stock_Interface'Class)
   is
   begin
      for Resource of Resource_Vector loop
         if Stock.Quantity (Resource) > 0.0 then
            To.Add (Resource, Stock.Quantity (Resource));
         end if;
      end loop;
   end Add_Stock;

   -----------------
   -- Clear_Stock --
   -----------------

   procedure Clear_Stock
     (Stock : in out Stock_Interface'Class)
   is
      procedure Clear (Resource : Carthage.Resources.Resource_Type);

      -----------
      -- Clear --
      -----------

      procedure Clear (Resource : Carthage.Resources.Resource_Type) is
      begin
         if Stock.Quantity (Resource) > 0.0 then
            Stock.Set_Quantity (Resource, 0.0);
         end if;
      end Clear;

   begin
      Carthage.Resources.Scan (Clear'Access);
   end Clear_Stock;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Stock            : in out Stock_Interface'Class;
      Resource         : not null access constant Resource_Class;
      Removed_Quantity : Resource_Quantity)
   is
   begin
      Stock.Set_Quantity
        (Resource, Stock.Quantity (Resource) - Removed_Quantity);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Stock            : in out Stock_Interface'Class;
      Resource         : not null access constant Resource_Class;
      Removed_Quantity : Natural)
   is
   begin
      Stock.Remove (Resource, Resource_Quantity (Removed_Quantity));
   end Remove;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From  : in out Stock_Interface'Class;
      Stock : Stock_Interface'Class)
   is
   begin
      for Resource of Resource_Vector loop
         if Stock.Quantity (Resource) > 0.0 then
            pragma Assert
              (Stock.Quantity (Resource) <= From.Quantity (Resource));
            From.Remove (Resource, Stock.Quantity (Resource));
         end if;
      end loop;
   end Remove_Stock;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access procedure (Resource : Resource_Type))
   is
   begin
      Db.Scan (Process);
   end Scan;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Stock   : Stock_Interface'Class;
      Process : not null access
        procedure (Resource : Resource_Type;
                   Quantity : Resource_Quantity))
   is
      procedure Check (Resource : Resource_Type);

      -----------
      -- Check --
      -----------

      procedure Check (Resource : Resource_Type) is
      begin
         if Stock.Quantity (Resource) > 0.0 then
            Process (Resource, Stock.Quantity (Resource));
         end if;
      end Check;

   begin
      Db.Scan (Check'Access);
   end Scan_Stock;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Stock   : Stock_Interface'Class;
      Process : not null access
        procedure (Resource : Resource_Type;
                   Quantity : Positive))
   is
      procedure Check (Resource : Resource_Type);

      -----------
      -- Check --
      -----------

      procedure Check (Resource : Resource_Type) is
         Quantity : constant Natural := Stock.Whole_Quantity (Resource);
      begin
         if Quantity > 0 then
            Process (Resource, Quantity);
         end if;
      end Check;

   begin
      Db.Scan (Check'Access);
   end Scan_Stock;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (Stock        : in out Stock_Record;
      Resource     : not null access constant Resource_Class;
      New_Quantity : Resource_Quantity)
   is
   begin
      Stock.Vector.Replace_Element (Resource, New_Quantity);
   end Set_Quantity;

end Carthage.Resources;
