package body Carthage.Houses is

   -----------
   -- Clear --
   -----------

   procedure Clear (Set : in out House_Set) is
   begin
      Set := 0;
   end Clear;

   ------------
   -- Insert --
   ------------

   procedure Insert (Set   : in out House_Set;
                     House : House_Type)
   is
   begin
      Set := Set or House.Set_Flag;
   end Insert;

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

end Carthage.Houses;
