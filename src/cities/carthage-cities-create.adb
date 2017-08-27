package body Carthage.Cities.Create is

   --------------
   -- New_City --
   --------------

   function New_City
     (Planet   : Carthage.Planets.Planet_Type;
      Tile     : Carthage.Tiles.Tile_Type;
      Structure : Carthage.Structures.Structure_Type;
      Owner    : Carthage.Houses.House_Type)
      return City_Type
   is
      procedure Create (City : in out City_Class);

      ------------
      -- Create --
      ------------

      procedure Create (City : in out City_Class) is
      begin
         City.Set_Name ("");
         City.Owner := Owner;
         City.Planet := Planet;
         City.Tile := Tile;
         City.Structure := Structure;
      end Create;

      City : constant City_Type :=
               Db.Create (Create'Access);
   begin

      Carthage.Tiles.Set_City (City.Tile, City);

      return City;

   end New_City;

   --------------
   -- New_City --
   --------------

   procedure New_City
     (Planet   : Carthage.Planets.Planet_Type;
      Tile     : Carthage.Tiles.Tile_Type;
      Structure : Carthage.Structures.Structure_Type;
      Owner    : Carthage.Houses.House_Type)
   is
      City : constant City_Type := New_City (Planet, Tile, Structure, Owner);
   begin
      pragma Unreferenced (City);
   end New_City;

end Carthage.Cities.Create;
