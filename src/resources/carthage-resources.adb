package body Carthage.Resources is

   ---------
   -- Add --
   ---------

   procedure Add
     (Stock          : in out Stock_Interface'Class;
      Resource       : not null access constant Resource_Class;
      Added_Quantity : Natural)
   is
   begin
      Stock.Set_Quantity
        (Resource, Stock.Quantity (Resource) + Added_Quantity);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Stock            : in out Stock_Interface'Class;
      Resource         : not null access constant Resource_Class;
      Removed_Quantity : Natural)
   is
   begin
      Stock.Set_Quantity
        (Resource, Stock.Quantity (Resource) - Removed_Quantity);
   end Remove;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access procedure (Resource : Resource_Type))
   is
   begin
      Db.Scan (Process);
   end Scan;

   procedure Scan_Stock
     (Stock   : Stock_Interface'Class;
      Process : not null access
        procedure (Resource : Resource_Type;
                   Quantity : Natural))
   is
      procedure Check (Resource : Resource_Type);

      -----------
      -- Check --
      -----------

      procedure Check (Resource : Resource_Type) is
      begin
         if Stock.Quantity (Resource) > 0 then
            Process (Resource, Stock.Quantity (Resource));
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
      New_Quantity : Natural)
   is
   begin
      Stock.Vector.Replace_Element (Resource, New_Quantity);
   end Set_Quantity;

end Carthage.Resources;
