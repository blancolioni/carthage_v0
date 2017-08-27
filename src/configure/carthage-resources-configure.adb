package body Carthage.Resources.Configure is

   ------------------------
   -- Configure_Resource --
   ------------------------

   procedure Configure_Resource
     (Config : Tropos.Configuration)
   is
      procedure Create (Resource : in out Resource_Class);

      ------------
      -- Create --
      ------------

      procedure Create (Resource : in out Resource_Class) is
      begin
         Resource.Create_With_Identity (Config.Config_Name);
         Resource.Price := Config.Get ("price");
      end Create;

   begin
      Db.Create (Create'Access);
   end Configure_Resource;

end Carthage.Resources.Configure;
