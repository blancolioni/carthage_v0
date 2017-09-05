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
         City.Create_With_Identity (Structure.Identifier);
         City.Set_Name (Structure.Name);
         City.Owner := Owner;
         City.Planet := Planet;
         City.Tile := Tile;
         City.Structure := Structure;
         Carthage.Houses.Clear (City.Seen);
      end Create;

      City : constant City_Type :=
               Db.Create (Create'Access);
   begin

      City.Tile.Update.Set_City (City);

      if not Planet.Has_Agora and then City.Is_Agora then
         declare
            procedure Set (P : in out Carthage.Planets.Planet_Class);

            ---------
            -- Set --
            ---------

            procedure Set (P : in out Carthage.Planets.Planet_Class) is
            begin
               P.Set_Agora (City);
            end Set;

         begin
            Carthage.Planets.Update
              (Planet, Set'Access);
         end;
      end if;

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
