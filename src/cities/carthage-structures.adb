package body Carthage.Structures is

   ----------------------
   -- Bonus_Production --
   ----------------------

   function Bonus_Production
     (Structure       : Structure_Record;
      Bonus_Structure : Structure_Type)
      return Natural
   is
      Result : Natural := 0;
   begin
      for Bonus of Structure.Bonus loop
         if Bonus.Bonus_Structure = Bonus_Structure then
            Result := Result + Bonus.Quantity;
         end if;
      end loop;
      return Result;
   end Bonus_Production;

   ------------
   -- Chance --
   ------------

   function Chance
     (Structure       : Structure_Record;
      Planet_Category : String;
      Terrain_Id      : String)
      return Natural
   is
      Key : constant String := Planet_Category & "-" & Terrain_Id;
   begin
      if Structure.Chances.Contains (Key) then
         return Structure.Chances.Element (Key);
      else
         return 0;
      end if;
   end Chance;

   ------------------------
   -- Harvest_Production --
   ------------------------

   function Harvest_Production
     (Structure : Structure_Record;
      Tile      : Carthage.Tiles.Tile_Type)
      return Natural
   is
      use Carthage.Terrain;
      Count : Natural := 0;
      Found : Boolean := False;
   begin
      if Tile.Has_City then
         return 0;
      end if;

      for Item of Structure.Production loop
         if Tile.Has_Terrain (Item.Terrain) then
            if not Found then
               Count := Item.Output;
               Found := True;
            else
               Count := Natural'Min (Count, Item.Output);
            end if;
         end if;
      end loop;
      return Count;
   end Harvest_Production;

end Carthage.Structures;
