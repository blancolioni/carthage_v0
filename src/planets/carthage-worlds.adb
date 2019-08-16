package body Carthage.Worlds is

   -----------------------
   -- Base_Land_Terrain --
   -----------------------

   function Base_Land_Terrain
     (World       : World_Record;
      Temperature : Carthage.Climate.Temperature_Range;
      Humidity    : Carthage.Climate.Humidity_Range)
      return Carthage.Terrain.Terrain_Type
   is
   begin
      for Item of World.Climate_Terrain loop
         if Temperature in Item.Temp_Low .. Item.Temp_High
           and then Humidity in Item.Humidity_Low .. Item.Humidity_High
         then
            return Item.Terrain;
         end if;
      end loop;
      return World.Base_Land;
   end Base_Land_Terrain;

   ----------------------------
   -- Scan_Terrain_Frequency --
   ----------------------------

   procedure Scan_Terrain_Frequency
     (World : World_Record;
      Process : not null access
        procedure (Terrain : Carthage.Terrain.Terrain_Type;
                   Frequency : Frequency_Range))
   is
   begin
      for Item of World.Terrain loop
         Process (Item.Terrain, Item.Frequency);
      end loop;
   end Scan_Terrain_Frequency;

end Carthage.Worlds;
