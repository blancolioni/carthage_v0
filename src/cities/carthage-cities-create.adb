package body Carthage.Cities.Create is

   --------------
   -- New_City --
   --------------

   function New_City
     (Planet   : Carthage.Planets.Planet_Type;
      Tile     : Carthage.Tiles.Tile_Type;
      Structure : Carthage.Structures.Structure_Type;
      Owner    : Carthage.Houses.House_Type;
      Health    : Health_Type;
      Loyalty   : Loyalty_Type)
      return City_Type
   is
      procedure Create (City : in out City_Class);

      ------------
      -- Create --
      ------------

      procedure Create (City : in out City_Class) is
      begin
         City.Create_With_Identity
           (Planet.Identifier
            & "-" & Carthage.Tiles.Position_Image (Tile.Position)
            & "-"
            & Structure.Identifier);

         City.Set_Name (Structure.Name);
         City.Owner := Owner;
         City.Planet := Planet;
         City.Tile := Tile;
         City.Structure := Structure;
         City.Loyalty := Loyalty;
         City.Health := Health;
         Carthage.Houses.Clear (City.Seen);

         if City.Is_Agora then
            for I in 1 .. Carthage.Resources.Last_Index loop
               declare
                  Resource : constant Carthage.Resources.Resource_Type :=
                    Carthage.Resources.Get (I);
                  Base_Price : constant Positive :=
                    Resource.Base_Price;
                  Buy_Price  : constant Positive :=
                    Natural'Max (Base_Price * 95 / 100, 1);
                  Sell_Price : constant Positive :=
                    Natural'Max (Base_Price * 105 / 100, Buy_Price + 1);
               begin
                  City.Prices.Append
                    (Agora_Resource_Record'
                       (Buy_Price  => Buy_Price,
                        Sell_Price => Sell_Price));
               end;
            end loop;
         end if;

      end Create;

      City : constant City_Type :=
               Db.Create (Create'Access);
   begin

      City.Tile.Update.Set_City (City);

      return City;

   end New_City;

   --------------
   -- New_City --
   --------------

   procedure New_City
     (Planet    : Carthage.Planets.Planet_Type;
      Tile      : Carthage.Tiles.Tile_Type;
      Structure : Carthage.Structures.Structure_Type;
      Owner     : Carthage.Houses.House_Type;
      Health    : Health_Type;
      Loyalty   : Loyalty_Type)
   is
      City : constant City_Type :=
               New_City (Planet, Tile, Structure, Owner, Health, Loyalty);
   begin
      pragma Unreferenced (City);
   end New_City;

end Carthage.Cities.Create;
