with Carthage.Import;

package body Carthage.Terrain.Configure is

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Config : Tropos.Configuration)
   is
      procedure Create (Terrain : in out Terrain_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Terrain : in out Terrain_Class) is
      begin
         Terrain.Create_With_Identity (Config.Config_Name);
         Terrain.Ocean := Config.Config_Name = "ocean";
         Terrain.Water := Terrain.Ocean or else Config.Get ("water");
         for Item of Config.Child ("Colors") loop
            Terrain.Cat_Info.Insert
              (Key      => Item.Config_Name,
               New_Item =>
                 Terrain_Category_Info'
                   (Color    =>
                        Carthage.Import.Palette_Color (Item.Value),
                    Road_Cost =>
                      Config.Child ("road-cost").Get (Item.Config_Name)));
         end loop;
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Terrain;

end Carthage.Terrain.Configure;
