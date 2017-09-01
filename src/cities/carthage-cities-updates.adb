with Memor.Element_Vectors;

with Carthage.Resources;

package body Carthage.Cities.Updates is

   procedure Execute_City_Production
     (City : in out City_Class);

   procedure Execute_Harvester_Production
     (City : in out City_Class);

   -----------------------------
   -- Execute_City_Production --
   -----------------------------

   procedure Execute_City_Production
     (City : in out City_Class)
   is
   begin
      if City.Structure.Is_Harvester then
         Execute_Harvester_Production (City);
      end if;
   end Execute_City_Production;

   ----------------------------------
   -- Execute_Harvester_Production --
   ----------------------------------

   procedure Execute_Harvester_Production
     (City : in out City_Class)
   is
      use Carthage.Planets;
      Tiles  : Surface_Tiles;
      package Output_Vectors is
        new Memor.Element_Vectors
          (Carthage.Resources.Resource_Record, Natural, 0);
      Output : Output_Vectors.Vector;
   begin
      Get_Tiles
        (City.Planet, City.Tile, 0, City.Structure.Radius + 1, null,
         Tiles);

      for I in 1 .. Tile_Count (Tiles) loop
         declare
            Prod : constant Carthage.Structures.Production_Array :=
                     City.Structure.Harvest_Production
                       (Tile => Get_Tile (Tiles, I));
         begin
            for Rec of Prod loop
               Output.Replace_Element
                 (Rec.Resource,
                  Output.Element (Rec.Resource) + Rec.Quantity);
            end loop;
         end;
      end loop;

      declare
         procedure Report
           (Resource : Carthage.Resources.Resource_Class;
            Quantity : Natural);

         ------------
         -- Report --
         ------------

         procedure Report
           (Resource : Carthage.Resources.Resource_Class;
            Quantity : Natural)
         is
         begin
            if Quantity > 0 then
               City.Log
                 ("on " & City.Planet.Name & " at"
                  & City.Tile.Position.X'Img & City.Tile.Position.Y'Img
                  & ":" & Natural'Image (Tile_Count (Tiles))
                  & " produce" & Quantity'Img & " "
                  & Resource.Name);
            end if;
         end Report;

      begin
         Output.Scan (Report'Access);
      end;
   end Execute_Harvester_Production;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production is
   begin
      Db.Iterate (Execute_City_Production'Access);
   end Execute_Production;

end Carthage.Cities.Updates;
