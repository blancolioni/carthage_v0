with Memor.Element_Vectors;

with Carthage.Resources;

package body Carthage.Cities.Updates is

   procedure Execute_City_Orders
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
                  Agora : constant City_Type := Order.Other_City;
                  Quantity : constant Natural :=
                               Natural'Min
                                 (Order.Quantity,
                                  Agora.Whole_Quantity (Order.Resource));
                  Cost     : constant Natural :=
                               Quantity * Order.Resource.Base_Price
                                 * 11 / 10;

               begin
                  if Quantity > 0 then
                     City.Log
                       ("buy" & Quantity'Img & " "
                        & Order.Resource.Name
                        & " for" & Cost'Img);
                     City.Owner.Update.Spend (Cost);
                     Agora.Owner.Update.Earn (Cost);
                     Agora.Update.Remove (Order.Resource, Quantity);
                     City.Add (Order.Resource, Quantity);
                     if City.Manager /= null then
                        City.Manager.On_Resource_Arrival
                          (Db.Reference (City.Reference),
                           Order.Resource, Quantity);
                     end if;
                  end if;
               end;
            when Sell =>
               declare
                  Agora    : constant City_Type := Order.Other_City;
                  Quantity : constant Natural := Order.Quantity;
                  Resource : constant Carthage.Resources.Resource_Type :=
                               Order.Resource;
                  Cost     : constant Natural :=
                               Quantity * Order.Resource.Base_Price
                                 * 9 / 10;

               begin
                  City.Log
                    ("sell" & Quantity'Img & " "
                     & Resource.Name
                     & " for" & Cost'Img);
                  City.Owner.Update.Earn (Cost);
                  Agora.Owner.Update.Spend (Cost);
                  Agora.Update.Add (Resource, Quantity);
                  City.Remove (Resource, Quantity);
               end;
            when Transfer =>
               City.Log
                 ("transfer" & Order.Quantity'Img & " "
                  & Order.Resource.Name
                  & " to " & Order.Other_City.Identifier);
               Order.Other_City.Update.Add (Order.Resource, Order.Quantity);
               City.Remove (Order.Resource, Order.Quantity);
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
         Quantity : constant Natural := City.Whole_Quantity (Resource);
      begin
         if Quantity > 0 then
            City.Log
              (Resource.Name & ":" & Quantity'Img);
         end if;
      end Report_Stock;

   begin
      if City.Structure.Is_Harvester then
         Execute_Harvester_Production (City);
      else
         City.Structure.Execute_Production
           (Stock      => City,
            Efficiency =>
              Float (City.Loyalty) / 100.0
            * Float (City.Health) / 100.0,
            Factor     => 0.1);
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
          (Carthage.Resources.Resource_Record, Resource_Quantity, 0.0);
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
               declare
                  Quantity : Resource_Quantity := Rec.Quantity;
               begin
                  Quantity :=
                    Resource_Quantity (Float (Quantity)
                                       * Float (City.Health) / 100.0
                                       * Float (City.Loyalty) / 100.0
                                       / 30.0);
                  Quantity := Quantity + Output.Element (Rec.Resource);
                  Output.Replace_Element (Rec.Resource, Quantity);
               end;
            end loop;
         end;
      end loop;

      declare
         procedure Add_Harvested_Resources
           (Resource : not null access constant
              Carthage.Resources.Resource_Class;
            Quantity : Resource_Quantity);

         -----------------------------
         -- Add_Harvested_Resources --
         -----------------------------

         procedure Add_Harvested_Resources
           (Resource : not null access constant
              Carthage.Resources.Resource_Class;
            Quantity : Resource_Quantity)
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
