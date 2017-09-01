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
      return Production_Array
   is
      use Carthage.Terrain;
      Count : Natural := 0;
      Result : Production_Array (1 .. 10);
   begin
      for Item of Structure.Production loop
         if Tile.Has_Terrain (Item.Terrain) then
            declare
               use type Carthage.Resources.Resource_Type;
               Index : Natural := 0;
            begin
               for I in 1 .. Count loop
                  if Result (I).Resource = Item.Resource then
                     Index := I;
                     exit;
                  end if;
               end loop;
               if Index = 0 then
                  Count := Count + 1;
                  Index := Count;
                  Result (Index) := (Item.Resource, 0);
               end if;

               Result (Index).Quantity :=
                 Result (Index).Quantity + Item.Output;
            end;
         end if;
      end loop;
      return Result (1 .. Count);
   end Harvest_Production;

end Carthage.Structures;
