package body Carthage.Terrain is

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Process : not null access procedure (Terrain : Terrain_Type))
   is
   begin
      Db.Scan (Process);
   end Scan;

   -------------------
   -- Terrain_Count --
   -------------------

   function Terrain_Count return Natural is
   begin
      return Db.Active_Count;
   end Terrain_Count;

end Carthage.Terrain;
