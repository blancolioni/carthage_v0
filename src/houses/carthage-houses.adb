package body Carthage.Houses is

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
