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
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Structure : Structure_Record;
      Stock     : in out Carthage.Resources.Stock_Interface'Class)
   is
      Ready : Boolean := True;
   begin
      for Item of Structure.Inputs loop
         if Stock.Quantity (Item.Resource) < Item.Quantity then
            Structure.Log ("insufficient " & Item.Resource.Identifier
                           & "; needed" & Natural'Image (Item.Quantity)
                           & " but have"
                           & Natural'Image (Stock.Quantity (Item.Resource)));
            Ready := False;
            exit;
         end if;
      end loop;

      if Ready then
         for Item of Structure.Inputs loop
            Stock.Remove (Item.Resource, Item.Quantity);
         end loop;
         for Item of Structure.Production loop
            Structure.Log ("creating" & Natural'Image (Item.Quantity)
                           & " " & Item.Resource.Identifier);
            Stock.Add (Item.Resource, Item.Quantity);
         end loop;
      end if;

   end Execute_Production;

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
                 Result (Index).Quantity + Item.Quantity;
            end;
         end if;
      end loop;
      return Result (1 .. Count);
   end Harvest_Production;

end Carthage.Structures;
