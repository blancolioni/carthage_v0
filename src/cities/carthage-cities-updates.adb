with Memor.Element_Vectors;

with Carthage.Resources;

package body Carthage.Cities.Updates is

   procedure Execute_City_Orders
     (City : in out City_Class);

   procedure Execute_City_Production
     (City : in out City_Class);

   procedure Execute_Harvester_Production
     (City : in out City_Class);

   -------------------------
   -- Execute_City_Orders --
   -------------------------

   procedure Execute_City_Orders
     (City : in out City_Class)
   is
   begin
      for Order of City.Orders loop
         case Order.Class is
            when Buy =>
               declare
                  Agora : constant City_Type :=
                            City_Type (City.Planet.Agora);
                  Quantity : constant Natural :=
                               Natural'Min
                                 (Order.Quantity,
                                  Agora.Quantity (Order.Resource));
                  Cost     : constant Natural :=
                               Quantity * Order.Resource.Base_Price
                                 * 11 / 10;

                  procedure Earn
                    (H : in out Carthage.Houses.House_Class);

                  procedure Spend
                    (H : in out Carthage.Houses.House_Class);

                  procedure Remove
                    (A : in out Carthage.Cities.City_Class);

                  ----------
                  -- Earn --
                  ----------

                  procedure Earn
                    (H : in out Carthage.Houses.House_Class)
                  is
                  begin
                     H.Earn (Cost);
                  end Earn;

                  ------------
                  -- Remove --
                  ------------

                  procedure Remove
                    (A : in out Carthage.Cities.City_Class)
                  is
                  begin
                     A.Remove (Order.Resource, Quantity);
                  end Remove;

                  -----------
                  -- Spend --
                  -----------

                  procedure Spend
                    (H : in out Carthage.Houses.House_Class)
                  is
                  begin
                     H.Spend (Cost);
                  end Spend;

               begin
                  if Quantity > 0 then
                     Carthage.Houses.Update (City.Owner, Spend'Access);
                     Carthage.Houses.Update (Agora.Owner, Earn'Access);
                     Db.Update (Agora.Reference, Remove'Access);
                     City.Add (Order.Resource, Quantity);
                  end if;
               end;
            when Sell =>
               null;
         end case;
      end loop;

      City.Orders.Clear;
   end Execute_City_Orders;

   -----------------------------
   -- Execute_City_Production --
   -----------------------------

   procedure Execute_City_Production
     (City : in out City_Class)
   is
      procedure Report_Stock
        (Resource : Carthage.Resources.Resource_Type);

      ------------------
      -- Report_Stock --
      ------------------

      procedure Report_Stock
        (Resource : Carthage.Resources.Resource_Type)
      is
         Quantity : constant Natural := City.Quantity (Resource);
      begin
         if Quantity > 0 then
            City.Planet.Log
              (City.Identifier & ": " & Resource.Name & ":" & Quantity'Img);
         end if;
      end Report_Stock;

   begin
      if City.Structure.Is_Harvester then
         Execute_Harvester_Production (City);
      else
         City.Structure.Execute_Production (City);
      end if;
      Carthage.Resources.Scan (Report_Stock'Access);
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
         procedure Add_Harvested_Resources
           (Resource : not null access constant
              Carthage.Resources.Resource_Class;
            Quantity : Natural);

         -----------------------------
         -- Add_Harvested_Resources --
         -----------------------------

         procedure Add_Harvested_Resources
           (Resource : not null access constant
              Carthage.Resources.Resource_Class;
            Quantity : Natural)
         is
         begin
            City.Add (Resource, Quantity);
         end Add_Harvested_Resources;

      begin
         Output.Scan (Add_Harvested_Resources'Access);
      end;
   end Execute_Harvester_Production;

   --------------------
   -- Execute_Orders --
   --------------------

   procedure Execute_Orders is
   begin
      Db.Iterate (Execute_City_Orders'Access);
   end Execute_Orders;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production is
   begin
      Db.Iterate (Execute_City_Production'Access);
   end Execute_Production;

end Carthage.Cities.Updates;
